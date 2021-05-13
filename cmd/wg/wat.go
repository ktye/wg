package main

import (
	"fmt"
	"io"
	"strings"
)

// convert wg ast to webassembly text format

func (m Module) wat(w io.Writer) {
	fmt.Fprintf(w, "(module\n")
	for _, f := range m.Funcs {
		f.wat(w)
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
	fmt.Fprintf(w, "\n")
	for _, st := range f.Body {
		st.wat(w)
	}
	fmt.Fprintf(w, ")\n")
}

func (a Assign) wat(w io.Writer) {
	if a.Mod != "=" && a.Mod != ":=" { // x += y -> x = x + y
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
}
