package wg

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

// convert wg ast to webassembly text format

func (m Module) Wat(ww io.Writer) {
	var w = newIndent(ww)
	fmt.Fprintf(w, "(module\n")
	for _, i := range m.Imports {
		i.wat(w)
	}
	if m.Memory != "" {
		fmt.Fprintf(w, "(memory (export \"memory\") %s)\n", m.Memory)
	}
	for _, g := range m.Globals {
		for i, s := range g.Name {
			t := g.Typs[i]
			var u string
			switch v := g.Expr[i].(type) {
			case Cast:
				u = v.Arg.(Literal).Value
			case Literal:
				u = v.Value
			default:
				panic("global value must be (cast-to) literal")
			}
			mut := string(t)
			if g.Const[i] == false {
				mut = "(mut " + string(t) + ")"
			}
			fmt.Fprintf(w, "(global $%s %s (%s.const %s))\n", s, mut, t, u)
		}
	}
	for _, f := range m.Funcs {
		f.wat(w)
	}
	tmax := 0
	for _, e := range m.Table {
		if n := e.Off + len(e.Names); n > tmax {
			tmax = n
		}
	}
	fmt.Fprintf(w, "(table %d funcref)\n", tmax)
	for _, e := range m.Table {
		fmt.Fprintf(w, "(elem (i32.const %d) func", e.Off)
		for i := range e.Names {
			fmt.Fprintf(w, " $%s", e.Names[i])
		}
		fmt.Fprintf(w, ")\n")
	}
	fmt.Fprintf(w, ")\n")
	w.Write(nil)
}
func (s Stmts) wat(w io.Writer) {
	for _, st := range s {
		st.wat(w)
	}
}
func (m Import) wat(w io.Writer) {
	//(import "wasi_unstable" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
	arglist := func(t []Type, r string) string {
		if len(t) == 0 {
			return ""
		}
		for _, s := range t {
			r += " " + string(s)
		}
		return r + ")"
	}
	arg := arglist(m.Arg, "(param")
	res := arglist(m.Res, "(result")
	fmt.Fprintf(w, "(import \"%s\" \"%s\" (func $%s.%s %s %s))\n", m.Package, m.Func, m.Package, m.Func, arg, res)
}
func (f Func) wat(w io.Writer) {
	fmt.Fprintf(w, "(func $%s", f.Name)
	if f.Exported {
		fmt.Fprintf(w, " (export \"%s\")", f.Name)
	}
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
	if len(f.Body) > 0 {
		if r, o := f.Body[len(f.Body)-1].(Return); o {
			r.Last = true
			f.Body[len(f.Body)-1] = r
		}
	}
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
		var get Expr = LocalGet(a.Name[0])
		if a.Glob[0] == true {
			get = GlobalGet(a.Name[0])
		}
		a.Expr = []Expr{Binary{
			X:  get,
			Y:  a.Expr[0],
			Op: Op{Name: strings.TrimSuffix(a.Mod, "="), Type: a.Typs[0]},
		}}
	}
	for _, e := range a.Expr {
		e.wat(w)
	}
	if a.Expr != nil {
		for i, n := range a.Name {
			if a.Glob[i] {
				fmt.Fprintf(w, "global.set $%s\n", n)
			} else {
				fmt.Fprintf(w, "local.set $%s\n", n)
			}
		}
	}
}
func (r Return) wat(w io.Writer) {
	for _, e := range r.List {
		e.wat(w)
	}
	if r.Last == false {
		fmt.Fprintln(w, "return")
	}
}
func (r Drop) wat(w io.Writer) {
	r.Expr.wat(w)
	fmt.Fprintln(w, "drop")
}
func (n Nop) wat(w io.Writer)       {}
func (l GlobalGet) wat(w io.Writer) { fmt.Fprintf(w, "global.get $%s\n", l) }
func (l GlobalGets) wat(w io.Writer) {
	for i := range l {
		fmt.Fprintf(w, "global.get $%s\n", l[i])
	}
}
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
	if c.Func == "panic" {
		fmt.Fprintln(w, "unreachable")
		return
	}
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
	// load (missing: i64.load{8,16,32}_[su])
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

	// wasm ops
	case "I32clz", "I64clz", "I32ctz", "I64ctz", "I32popcnt", "I64popcnt", "F64abs", "F64sqrt", "F64ceil", "F64floor", "F64nearest", "F64min", "F64max", "F64copysign", "I64reinterpret_f64", "F64reinterpret_i64", "I32reinterpret_f32", "F32reinterpret_i32":
		op = fmt.Sprintf("%s.%s", strings.ToLower(c.Func[:3]), c.Func[3:])

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
func (i If) wat(w io.Writer) {
	i.If.wat(w)
	fmt.Fprintln(w, "if")
	for _, t := range i.Then {
		t.wat(w)
	}
	if i.Else != nil {
		fmt.Fprintln(w, "else")
		for _, e := range i.Else {
			e.wat(w)
		}
	}
	fmt.Fprintln(w, "end")
}
func (s Switch) wat(w io.Writer) {
	for i := 0; i < 1+len(s.Case); i++ {
		fmt.Fprintln(w, "block")
	}
	s.E.wat(w)
	t := "br_table"
	for i := range s.Case {
		t += " " + strconv.Itoa(i)
	}
	if s.Def != nil {
		t += " " + strconv.Itoa(len(s.Case))
	}
	fmt.Fprintln(w, t)
	fmt.Fprintln(w)
	for i := 1 + len(s.Case); i != 0; i-- {
		if i == 1 && s.Def != nil {
			s.Def.wat(w)
		} else if i > 1 {
			s.Case[1+len(s.Case)-i].wat(w)
		}
		fmt.Fprintf(w, "br %d\n", i-1)
		fmt.Fprintln(w, "end")
	}
}
func (f For) wat(w io.Writer) {
	l1, l2 := "", ""
	if f.Label != "" {
		l1, l2 = "$"+f.Label+":1", "$"+f.Label+":2"
	}
	fmt.Fprintf(w, "block %s\n", l1)
	fmt.Fprintf(w, "loop %s\n", l2)
	if f.Cond != nil {
		f.Cond.wat(w)
		fmt.Fprintf(w, "i32.eqz br_if 1\n")
	}
	f.Body.wat(w)
	if f.Post != nil {
		f.Post.wat(w)
	}
	fmt.Fprintln(w, "br 0")
	fmt.Fprintln(w, "end")
	fmt.Fprintln(w, "end")
}
func (b Branch) wat(w io.Writer) {
	// assume for{ if{ break|continue } } => (block(loop(if(then br 1|2))))
	if b.Label == "" {
		if b.Break {
			fmt.Fprintf(w, "br 2\n")
		} else {
			fmt.Fprintf(w, "br 1\n")
		}
	} else {
		l := "$" + b.Label
		if b.Break {
			l += ":1"
		} else {
			l += ":2"
		}
		fmt.Fprintf(w, "br %s\n", l)
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
		"+":  ".add",
		"-":  ".sub",
		"*":  ".mul",
		"/":  ".div*",
		"%":  "rem*",
		"&":  "and",
		"&&": "and",
		"|":  "or",
		"||": "or",
		"^":  "xor",
		"<<": "shl",
		">>": "shr*",
		"!":  "eqz",
		"==": ".eq",
		"!=": ".ne",
		"<":  ".lt*",
		">":  ".gt*",
		"<=": ".le*",
		">=": ".ge*",
	}
	for a, b := range m {
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

	wasmcst = map[string]string{
		"i32<-i32": "", "i32<-u32": "", "i32<-i64": "i32.wrap_i64", "i32<-u64": "i32.wrap_i64", "i32<-f32": "i32.trunc_f32_s", "i32<-f64": "i32.trunc_f64_s",
		"u32<-i32": "", "u32<-u32": "", "u32<-i64": "i32.wrap_i64", "u32<-u64": "i32.wrap_i64", "u32<-f32": "i32.trunc_f32_u", "u32<-f64": "i32.trunc_f64_u",
		"i64<-i32": "i64.extend_i32_s", "i64<-u32": "i64.extend_i32_s", "i64<-i64": "", "i64<-u64": "", "i64<-f32": "i64.trunc_f32_s", "i64<-f64": "i64.trunc_f64_s",
		"u64<-i32": "i64.extend_i32_u", "u64<-u32": "i64.extend_i32_u", "u64<-i64": "", "u64<-u64": "", "u64<-f32": "i64.trunc_f32_u", "u64<-f64": "i64.trunc_f64_u",
		"f32<-i32": "f32.convert_i32_s", "f32<-u32": "f32.convert_i32_u", "f32<-i64": "f32.convert_i64_s", "f32<-u64": "f32.convert_i64_u", "f32<-f32": "", "f32<-f64": "f32.demote_f64",
		"f64<-i32": "f64.convert_i32_s", "f64<-u32": "f64.convert_i32_u", "f64<-i64": "f64.convert_i64_s", "f64<-u64": "f64.convert_i64_u", "f64<-f32": "f64.promote_f32", "f64<-f64": "",
	}
	// missing:
	//   i{32,64}.extend_{8,16}_s
	//   i{32,64}.trunc_sat_f{32,64}_[su]
}

type indent struct {
	io.Writer
	l int
	s string
}

func newIndent(w io.Writer) *indent {
	i := indent{Writer: w}
	return &i
}
func (w *indent) Write(p []byte) (n int, err error) {
	s := string(p)

	w.s, s = tee(w.s, s)
	if strings.HasSuffix(w.s, "\n") && s == ")\n" {
		w.s = strings.TrimSuffix(w.s, "\n")
	}
	if w.s != "" {
		if strings.HasSuffix(w.s, "\n") {
			w.s += strings.Repeat(" ", w.l)
		}
		_, err = w.Writer.Write([]byte(w.s))
		if err != nil {
			return 0, err
		}
	}
	w.s = s

	switch s {
	case "if\n":
		w.l++
	case "end\n", ")\n":
		w.l--
		if w.l < 0 {
			w.l = 0
		}
	}
	if strings.HasPrefix(s, "block ") || strings.HasPrefix(s, "loop ") {
		w.l++
	}
	if strings.HasPrefix(s, "(func ") {
		w.l = 1
	}
	return len(p), nil
}
func tee(a, b string) (string, string) {
	if strings.HasPrefix(a, "local.set $") && strings.HasPrefix(b, "local.get $") {
		if v := a[11:]; v == b[11:] {
			return "", "local.tee $" + v
		}
	}
	return a, b
}
