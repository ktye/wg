package wg

import (
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"
)

// convert wg ast to webassembly text format

var NoSys bool

func (m Module) Wat(w io.Writer) {
	fmt.Fprintln(w, "(module")
	if NoSys == false {
		fmt.Fprintf(w, `(import "env" "Exit"  (func $Exit  (param i32)))
(import "env" "Args"  (func $Args  (result i32)))
(import "env" "Arg"   (func $Arg   (param i32) (param i32) (result i32)))
(import "env" "Read"  (func $Read  (param i32) (param i32) (param i32) (result i32)))
(import "env" "Write" (func $Write (param i32) (param i32) (param i32) (param i32) (result i32)))
(import "env" "ReadIn" (func $ReadIn (param i32) (param i32) (result i32)))
(import "env" "Native" (func $Native (param i64) (param i64) (result i64)))
`)
	}

	if TryCatch {
		// fmt.Fprintln(w, "(tag $panic)") // proposal
		fmt.Fprintln(w, "(exception_type $panic)") // wavm
	}
	if m.Memory != "" {
		fmt.Fprintf(w, "(memory $a (export \"memory\") %s)\n", m.Memory)
	}
	if m.Memory2 != "" && MultiMemory {
		fmt.Fprintf(w, "(memory $b %s)\n", m.Memory2)
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
			mut := t.String()
			if g.Const[i] == false {
				mut = "(mut " + t.String() + ")"
			}
			//fmt.Fprintf(w, "(global $%s (export \"%s\") %s (%s.const %s))\n", s, s, mut, t, u)
			fmt.Fprintf(w, "(global $%s %s (%s.const %s))\n", s, mut, t, u)
		}
	}
	for _, d := range m.Data {
		q := strings.Replace(strconv.Quote(d.Data), `\x`, `\`, -1)
		fmt.Fprintf(w, "(data (i32.const %d) %s)\n", d.Off, q)
	}
	syscalls = make(map[string]bool)
	for _, f := range m.Funcs {
		f.Exported = m.Exports[f.Name] || m.exportAll
		if f.Name == "main" && Nomain == true {
			continue
		}
		f.wat(w)
	}
	nosys(w)

	if len(m.Table) > 1 {
		tmax := 0
		for _, e := range m.Table {
			if n := e.Off + len(e.Names); n > tmax {
				tmax = n
			}
		}
		fmt.Fprintf(w, "(table (export \"table\") %d funcref)\n", tmax)
		for _, e := range m.Table {
			fmt.Fprintf(w, "(elem (i32.const %d) func", e.Off)
			for i := range e.Names {
				fmt.Fprintf(w, " $%s", e.Names[i])
			}
			fmt.Fprintf(w, ")\n")
		}
	}
	fmt.Fprintf(w, ")\n")
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
			r += " " + s.String()
		}
		return r + ")"
	}
	arg := arglist(m.Arg, "(param")
	res := arglist(m.Res, "(result")
	fmt.Fprintf(w, "(import \"%s\" \"%s\" (func $%s.%s %s %s))\n", m.Package, m.Func, m.Package, m.Func, arg, res)
}

type fw struct {
	io.Writer
	f Func // access to current function (e.g. return types)
}

func (f Func) wat(w io.Writer) {
	name := f.Name
	if name == "main" {
		name = "_start"
		f.Exported = true
	}
	fmt.Fprintf(w, "(func $%s", name)
	if f.Exported {
		fmt.Fprintf(w, " (export \"%s\")", name)
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
	var buf bytes.Buffer
	fw := fw{Writer: &buf, f: f}
	try := TryCatch && f.Defer != nil
	if try {
		fmt.Fprintf(fw, "try\n")
	}
	for _, st := range f.Body {
		st.wat(fw)
	}
	if try {
		fmt.Fprintf(fw, "catch_all\n")
		f.Defer.wat(fw)
		fmt.Fprintf(fw, "end\n")
	}
	b := optimize(buf.Bytes())
	w.Write(b)
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
		for i := len(a.Name) - 1; i >= 0; i-- {
			n := a.Name[i]
			if n == "_" {
				fmt.Fprintf(w, "drop\n")
			} else if a.Glob[i] {
				fmt.Fprintf(w, "global.set $%s\n", n)
			} else {
				fmt.Fprintf(w, "local.set $%s\n", n)
			}
		}
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
	s := strings.ToLower(l.Value)
	fmt.Fprintf(w, "%s.const %s\n", l.Type, s)
}
func (c Call) wat(w io.Writer) {
	if c.Func == "panic" {
		if TryCatch {
			fmt.Fprintln(w, "throw $panic")
		} else {
			fmt.Fprintln(w, "unreachable")
		}
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

	case "I32B":
		return //bool2i32 (nop)

	// store
	case "SetI8", "SetI16":
		op = fmt.Sprintf("i32.store%s", c.Func[4:])
	case "SetI32", "SetI64", "SetF32", "SetF64":
		op = fmt.Sprintf("%c%s.store", c.Func[3]+32, c.Func[4:])

	// wasm ops
	case "I32clz", "I64clz", "I32ctz", "I64ctz", "I32popcnt", "I64popcnt", "F64abs", "F64sqrt", "F64ceil", "F64floor", "F64nearest", "F64min", "F64max", "F64copysign", "I64reinterpret_f64", "F64reinterpret_i64", "I32reinterpret_f32", "F32reinterpret_i32":
		op = fmt.Sprintf("%s.%s", strings.ToLower(c.Func[:3]), c.Func[3:])

	// memory / bulk memory
	case "Memorysize", "Memorygrow", "Memorycopy", "Memoryfill":
		op = fmt.Sprintf("memory.%s", c.Func[6:])
	case "Memorycopy2":
		op = "memory.copy $b $a"
		if MultiMemory == false {
			op = "drop\ndrop\ndrop\nunreachable"
		}
	case "Memorycopy3":
		op = "memory.copy $a $b"
		if MultiMemory == false {
			op = "drop\ndrop\ndrop\nunreachable"
		}
	case "Memorysize2", "Memorygrow2":
		op = fmt.Sprintf("memory.%s $b", c.Func[6:len(c.Func)-1])
		if MultiMemory == false {
			op = "unreachable\ni32.const 0"
		}

	default:
		// if simd(c.Func, w) { return; }
		syscall(c.Func)

		// normal function call
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
	th, el, name, typ, ret := i.value()
	if ret {
		typ = w.(fw).f.Rets[0]
	}
	i.If.wat(w)
	fmt.Fprint(w, "if")
	if typ != "" {
		fmt.Fprintf(w, " (result %s)", typ)
	}
	fmt.Fprintln(w)
	for _, t := range th {
		t.wat(w)
	}
	if el != nil {
		fmt.Fprintln(w, "else")
		for _, e := range el {
			e.wat(w)
		}
	}
	fmt.Fprintln(w, "end")
	if name != "" {
		fmt.Fprintf(w, "local.set $%s\n", name)
	} else if ret {
		fmt.Fprintln(w, "return")
	}
}
func (i If) value() (th Stmts, el Stmts, name string, typ Type, ret bool) {
	var r []Stmts
	var ok bool
	if i.Else == nil {
		return i.Then, i.Else, "", "", false
	}
	r, name, typ, ret, ok = valueStatements([]Stmts{i.Then, i.Else})
	th, el = i.Then, i.Else
	if ok {
		th, el = r[0], r[1]
	} else {
		name, typ, ret = "", "", false
	}
	return
}

// valueStatements tests if multiple statements of an if/else or switch all end with an assigment
// to a local variable or a return. The assigment or return can be moved outwards.
func valueStatements(v []Stmts) (r []Stmts, name string, typ Type, ret bool, ok bool) {
	las := func(stmts Stmts) (Stmts, string, Type, bool, bool) {
		if len(stmts) > 0 {
			st := stmts[len(stmts)-1]
			if a, o := st.(Assign); o && len(a.Name) == 1 && a.Mod == "=" && a.Glob[0] == false {
				r := make(Stmts, len(stmts))
				copy(r, stmts[:len(stmts)-1])
				r[len(r)-1] = a.Expr[0]
				return r, a.Name[0], a.Typs[0], false, true
			}

			if rt, o := st.(Return); o && len(rt) == 1 {
				r := make(Stmts, len(stmts))
				copy(r, stmts[:len(stmts)-1])
				r[len(r)-1] = rt[0]
				return r, "", "", true, true
			}

		}
		return nil, "", "", false, false
	}
	s, name, typ, ret, ok := las(v[0])
	if ok == false {
		r, ret, ok = v, false, false
		return
	}
	r = append(r, s)
	for _, st := range v[1:] {
		s, n, t, rt, o := las(st)
		if o && n == name && t == typ && rt == ret {
			r = append(r, s)
		} else {
			r, ret, ok = v, false, false
			return
		}
	}
	return r, name, typ, ret, ok
}
func (s Switch) wat(w io.Writer) {
	cs, def, name, typ, ret := s.value()
	if ret {
		typ = w.(fw).f.Rets[0]
	}
	for i := 0; i < 2+len(cs); i++ {
		fmt.Fprint(w, "block")
		if typ != "" && i == 0 {
			fmt.Fprintf(w, " (result %s)", typ)
		}
		fmt.Fprintln(w)
	}
	s.E.wat(w)
	t := "br_table"
	for i := range cs {
		t += " " + strconv.Itoa(i)
	}
	t += " " + strconv.Itoa(len(cs))
	fmt.Fprintln(w, t)
	for i := 1 + len(cs); i != 0; i-- {
		fmt.Fprintln(w, "end")
		if i == 1 && def != nil {
			def.wat(w)
		} else if i > 1 {
			cs[1+len(cs)-i].wat(w)
		}
		fmt.Fprintf(w, "br %d\n", i-1)
	}
	fmt.Fprintln(w, "end")
	if name != "" {
		fmt.Fprintf(w, "local.set $%s\n", name)
	} else if ret {
		fmt.Fprintln(w, "return")
	}
}
func (s Switch) value() (cs []Stmts, def Stmts, name string, typ Type, ret bool) {
	var r []Stmts
	var ok bool
	if s.Def == nil {
		return s.Case, s.Def, "", "", false
	}
	r, name, typ, ret, ok = valueStatements(append(s.Case, s.Def))
	cs, def = s.Case, s.Def
	if ok {
		cs, def = r[:len(r)-1], r[len(r)-1]
	} else {
		name, typ, ret = "", "", false
	}
	return
}
func (f For) wat(w io.Writer) {
	if f.Simple {
		f.simple(w)
		return
	}
	l1, l2 := "", ""
	if f.Label != "" {
		l1, l2 = " $"+f.Label+":1", " $"+f.Label+":2"
	}
	fmt.Fprintf(w, "block%s\n", l1)
	fmt.Fprintf(w, "loop%s\n", l2)
	if f.Cond != nil {
		f.Cond.wat(w)
		fmt.Fprintf(w, "i32.eqz\nbr_if 1\n")
	}
	f.Body.wat(w)
	if f.Post != nil {
		f.Post.wat(w)
	}
	fmt.Fprintln(w, "br 0")
	fmt.Fprintln(w, "end")
	fmt.Fprintln(w, "end")
}
func (f For) simple(w io.Writer) {
	fmt.Fprintf(w, "loop\n")
	l := f.Body[len(f.Body)-1]
	if b, o := l.(Branch); o {
		if b.Break {
			panic("simple loop must have continue as last statement")
		}
		f.Body[:len(f.Body)-1].wat(w) // strip continue
	} else {
		panic("simple loop must have continue as last statement")
	}
	if f.Post != nil {
		f.Post.wat(w)
	}
	f.Cond.wat(w)
	fmt.Fprintf(w, "br_if 0\nend\n")
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
func (p Printf) wat(w io.Writer) {} // ignored in wasm

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
		wasmops["u32&^"] = "i32.const -1\ni32.xor\ni32.and"
		wasmops["i32&^"] = "i32.const -1\ni32.xor\ni32.and"
		wasmops["i64&^"] = "i64.const -1\ni64.xor\ni64.and"
		wasmops["u64&^"] = "i64.const -1\ni64.xor\ni64.and"
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

var syscalls map[string]bool

func syscall(s string) {
	switch s {
	case "Exit", "Arg", "Args", "Read", "Write", "ReadIn", "Native":
		syscalls[s] = true
	}
}
func nosys(w io.Writer) {
	if NoSys == false {
		return
	} // see ./module.go
	m := map[string]string{
		"Exit":   "(func $Exit (param i32))",
		"Arg":    "(func $Arg (param i32) (param i32) (result i32) i32.const 0)",
		"Args":   "(func $Args (result i32) i32.const 0)",
		"Read":   "(func $Read (param i32) (param i32) (param i32) (result i32) i32.const 0)",
		"Write":  "(func $Write (param i32) (param i32) (param i32) (param i32) (result i32) i32.const 0)",
		"ReadIn": "(func $ReadIn (param i32) (param i32) (result i32) i32.const 0)",
		"Native": "(func $Native (param i64) (param i64) (result i64) i64.const 0)",
	}
	for _, s := range strings.Split("Exit Arg Args Read Write ReadIn Native", " ") {
		if syscalls[s] {
			fmt.Fprintln(w, m[s])
		}
	}
}

func optimize(b []byte) (r []byte) {
	if len(b) > 0 && b[len(b)-1] == 10 { // prevent last empty word
		b = b[:len(b)-1]
	}
	w := bytes.Split(b, []byte{10})
	s := make([]string, len(w))
	for i := range w {
		s[i] = string(w[i])
	}
	f := []func([]string) []string{optRet, optTee, optNot0, optWhileLts, indent}
	for i := range f {
		s = f[i](s)
	}
	for i, x := range s {
		if i > 0 {
			r = append(r, 10)
		}
		r = append(r, []byte(x)...)
	}
	return r
}
func optRet(w []string) []string {
	if len(w) > 0 && w[len(w)-1] == "return" {
		w = w[:len(w)-1]
	}
	return w
}
func optTee(w []string) []string {
	j := 0
	for i := 0; i < len(w); i++ {
		if i < len(w)-1 {
			a, b := w[i], w[i+1]
			if strings.HasPrefix(a, "local.set") && strings.HasPrefix(b, "local.get") {
				if v := a[10:]; v == b[10:] {
					i++
					w[i] = "local.tee " + v
				}
			}
		}
		w[j] = w[i]
		j++
	}
	return w[:j]
}
func optNot0(w []string) []string { return replace(w, "i32.const 0;i32.ne;if", "if") }
func optWhileLts(w []string) []string {
	return replace(w, "i32.lt_s;i32.eqz;br_if 1", "i32.ge_s;br_if 1")
}
func replace(w []string, pat, rep string) []string {
	p, r := strings.Split(pat, ";"), strings.Split(rep, ";")
	if len(r) > len(p) {
		panic("long replacement")
	}
	k := 0
f:
	for i := 0; i < len(w); i++ {
		if i < 1+len(w)-len(p) {
			for j := 0; j < len(p); j++ {
				if w[i+j] != p[j] {
					break
				}
				if j == len(p)-1 {
					for n := 0; n < len(r); n++ {
						w[k+n] = r[n]
					}
					i += len(p) - 1
					k += len(r)
					continue f
				}
			}
		}
		w[k] = w[i]
		k++
	}
	return w[:k]
}

func indent(w []string) []string {
	l := 1
	for i, s := range w {
		if s == "else" || s == "end" || s == "catch_all" {
			l--
			if l < 0 {
				l = 0
			}
		}
		b := strings.Repeat(" ", l)
		w[i] = b + s
		if strings.HasPrefix(s, "if") || s == "else" || strings.HasPrefix(s, "block") || strings.HasPrefix(s, "loop") || strings.HasPrefix(s, "try") || strings.HasPrefix(s, "catch_all") {
			l++
		}
	}
	return w
}
