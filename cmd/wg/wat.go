package main

import (
	"fmt"
	"io"
	"strings"
)

// convert wg ast to webassembly text format

func (m Module) wat(w io.Writer) {
	fmt.Fprintf(w, "(module\n")
	if m.Memory != "" {
		fmt.Fprintf(w, "(memory (export \"memory\") %s)\n", m.Memory)
	}
	for _, f := range m.Funcs {
		f.wat(w)
	}
	for _, e := range m.Table {
		fmt.Fprintf(w, "(elem (i32.const %d) func", e.Off)
		for i := range e.Names {
			fmt.Fprintf(w, " $%s", e.Names[i])
		}
		fmt.Fprintf(w, ")\n")
	}
	fmt.Fprintf(w, ")\n")
}

func (f Func) wat(w io.Writer) {
	fmt.Fprintf(w, "(func $%s", f.Name)
	for _, a := range f.Args {
		fmt.Fprintf(w, " (param $%s %s)", a.Name, a.Type)
	}
	for _, a := range f.Rets {
		fmt.Fprintf(w, " (result %s)", a)
	}
	for _, a := range f.Locs {
		fmt.Fprintf(w, " (local $%s %s)", a.Name, a.Type)
	}
	fmt.Fprintf(w, "\n")
	for _, st := range f.Body {
		st.wat(w)
	}
	fmt.Fprintf(w, ")\n")
}

func (a Assign) wat(w io.Writer) {
	if a.Mod != "" && a.Mod != "=" && a.Mod != ":=" { // x += y -> x = x + y
		if len(a.Expr) != 1 || len(a.Name) != 1 {
			panic("modified assignment multiple lhs/rhs")
		}
		a.Expr = []Expr{Binary{
			X:  LocalGet(a.Name[0]),
			Y:  a.Expr[0],
			Op: Op{Name: strings.TrimSuffix(a.Mod, "="), Type: a.Type},
		}}
	}
	for _, e := range a.Expr {
		e.wat(w)
	}
	for _, n := range a.Name {
		fmt.Fprintf(w, "local.set $%s\n", n)
	}
}
func (r Return) wat(w io.Writer) {
	for _, e := range r {
		e.wat(w)
	}
	fmt.Fprintln(w, "return")
}
func (r Drop) wat(w io.Writer) {
	r.Expr.wat(w)
	fmt.Fprintln(w, "drop")
}
func (n Nop) wat(w io.Writer)      {}
func (l LocalGet) wat(w io.Writer) { fmt.Fprintf(w, "local.get $%s\n", l) }
func (l LocalGets) wat(w io.Writer) {
	for i := range l {
		fmt.Fprintf(w, "local.get $%s\n", l[i])
	}
}
func (u Unary) wat(w io.Writer) {
	s := string(u.Op.Type) + u.Op.Name
	op, ok := wasmops[s]
	if ok == false {
		panic("unknown wasm op: " + s)
	}
	if u.Op.Name == "-" {
		if strings.HasPrefix(string(u.Op.Type), "f") {
			op = string(u.Op.Type) + ".neg"
		} else { // -x -> 0-x
			fmt.Fprintf(w, "%s.const 0\n", u.Op.Type)
		}
	}
	u.X.wat(w)
	fmt.Fprintln(w, op)
}
func (b Binary) wat(w io.Writer) {
	s := string(b.Op.Type) + b.Op.Name
	op, ok := wasmops[s]
	if ok == false {
		panic("unknown wasm op: " + s)
	}
	b.X.wat(w)
	b.Y.wat(w)
	fmt.Fprintln(w, op)
}
func (l Literal) wat(w io.Writer) {
	fmt.Fprintf(w, "%s.const %s\n", l.Type, l.Value)
}
func (c Call) wat(w io.Writer) {
	for i := range c.Args {
		c.Args[i].wat(w)
	}
	c.call(w)
}
func (c CallIndirect) wat(w io.Writer) {
	for i := range c.Args {
		c.Args[i].wat(w)
	}
	c.Func.wat(w)
	fmt.Fprintf(w, "call_indirect")
	for _, t := range c.ArgType {
		fmt.Fprintf(w, " (param %s)", t)
	}
	for _, t := range c.ResType {
		fmt.Fprintf(w, " (result %s)", t)
	}
	fmt.Fprintln(w)
}
func (c Call) call(w io.Writer) {
	var op string
	switch c.Func {
	// load
	case "I8", "U8", "I16", "U16":
		sign := 's'
		if c.Func[0] == 'U' {
			sign = 'u'
		}
		op = fmt.Sprintf("i32.load%s_%c", c.Func[1:], sign)
	case "I32", "U32", "I64", "U64":
		op = fmt.Sprintf("i%s.load", c.Func[1:])
	case "F32", "F64":
		op = fmt.Sprintf("f%s.load", c.Func[1:])

	// store
	case "SetI8", "SetI16":
		op = fmt.Sprintf("i32.store%s", c.Func[4:])
	case "SetI32", "SetI64", "SetF32", "SetF64":
		op = fmt.Sprintf("%c%s.store", c.Func[3]+32, c.Func[4:])

	default: // normal function call
		fmt.Fprintf(w, "call $%s\n", c.Func)
		return
	}
	w.Write([]byte(op + "\n"))
}
func (c Cast) wat(w io.Writer) {
	c.Arg.wat(w)
	cnv := string(c.Dst + "<-" + c.Src)
	cst, o := wasmcst[cnv]
	if o == false {
		panic("unknown type conversion: " + cnv)
	}
	if cst != "" {
		fmt.Fprintln(w, cst)
	}
}

var wasmcst map[string]string
var wasmops map[string]string

func init() {
	// iunary:  clz | ctz | popcnt
	// ibinary: add | sub | mul | div_sx | rem_sx | and | or | xor | shl | shr_sx | rotl | rotr |
	// funary:  abs | neg | sqrt | ceil | floor | trunc | nearest
	// fbinary: add | sub | mul | div | min | max | copysign
	// itest:   eqz
	// irel:    eq | ne | lt_sx | gt_sx | le_sx | ge_sx
	// frel:   eq | ne | lt | gt | le | ge
	wasmops = make(map[string]string)
	m := map[string]string{ // .(int+float) *(sign)
		"+":                       ".add",
		"-":                       ".sub",
		"*":                       ".mul",
		"/":                       ".div*",
		"%":                       "rem*",
		"&":                       "and",
		"&&":                      "and",
		"|":                       "or",
		"||":                      "or",
		"^":                       "xor",
		"<<":                      "shl",
		">>":                      "shr*",
		"!":                       "eqz",
		"==":                      ".eq",
		"!=":                      ".ne",
		"<":                       ".lt*",
		">":                       ".gt*",
		"<=":                      ".le*",
		">=":                      ".ge*",
		"u32bits.LeadingZeros32":  "i32.clz",
		"u64bits.LeadingZeros64":  "i64.clz",
		"u32bits.TrailingZeros32": "i32.ctz",
		"u64bits.TrailingZeros64": "i64.ctz",
		"u32bits.OnesCount32":     "i32.popcnt",
		"u64bits.OnesCount64":     "i64.popcnt",
		"f64math.Abs":             "f64.abs",
		"f64math.Sqrt":            "f64.sqrt",
		"f64math.Ceil":            "f64.ceil",
		"f64math.Floor":           "f64.floor",
		"f64math.Trunc":           "f64.trunc",
		"f64math.Round":           "f64.nearest", //?
		"f64math.Min":             "f64.min",
		"f64math.Max":             "f64.max",
	}
	for a, b := range m {
		if strings.HasPrefix(a, "bits") || strings.HasPrefix(a, "math") {
			wasmops[a] = b
		} else {
			c := strings.TrimPrefix(b, ".")
			ux, ix := "", ""
			if strings.HasSuffix(c, "*") {
				c = strings.TrimSuffix(c, "*")
				ux, ix = "_u", "_s"
			}
			wasmops["u32"+a] = "i32." + c + ux
			wasmops["i32"+a] = "i32." + c + ix
			wasmops["u64"+a] = "i64." + c + ux
			wasmops["i64"+a] = "i64." + c + ix
			if strings.HasPrefix(b, ".") {
				wasmops["f32"+a] = "f32." + c
				wasmops["f64"+a] = "f64." + c
			}
		}
	}

	wasmcst = map[string]string{
		"i32<-i32": "", "i32<-u32": "", "i32<-i64": "i32.wrap_i64", "i32<-u64": "i32.wrap_i64", "i32<-f32": "i32.trunc_f32_s", "i32<-f64": "i32.trunc_f64_s",
		"u32<-i32": "", "u32<-u32": "", "u32<-i64": "i32.wrap_i64", "u32<-u64": "i32.wrap_i64", "u32<-f32": "i32.trunc_f32_u", "u32<-f64": "i32.trunc_f64_u",
		"i64<-i32": "i64.extend_i32_s", "i64<-u32": "i64.extend_i32_s", "i64<-i64": "", "i64<-u64": "", "i64<-f32": "i64.trunc_f32_s", "i64<-f64": "i64.trunc_f64_s",
		"u64<-i32": "i64.extend_i32_u", "u64<-u32": "i64.extend_i32_u", "u64<-i64": "", "u64<-u64": "", "u64<-f32": "i64.trunc_f32_u", "u64<-f64": "i64.trunc_f64_u",
		"f32<-i32": "f32.convert_i32_s", "f32<-u32": "f32.convert_i32_u", "f32<-i64": "f32.convert_i64_s", "f32<-u64": "f32.convert_i64_u", "f32<-f32": "", "f32<-f64": "f32.demote_f64",
		"f64<-i32": "f64.convert_i32_s", "f64<-u32": "f64.convert_i32_u", "f64<-i64": "f64.convert_i64_s", "f64<-u64": "f64.convert_i64_u", "f64<-f32": "f64.promote_f32", "f64<-f64": "",
	}
}
