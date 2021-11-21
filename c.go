package wg

import (
	"bytes"
	"fmt"
	"io"
	"sort"
	"strconv"
	"strings"

	"github.com/ktye/wg/dumbindent"
)

// convert wg ast to C
var gtyp map[string]Type
var ltyp map[string]Type
var ftyp map[string][]Type
var cstk []Type

func (m Module) C(out io.Writer) {
	gtyp = make(map[string]Type)
	ftyp = make(map[string][]Type)
	var buf bytes.Buffer
	w := &buf
	w.Write([]byte(chead))

	for _, f := range m.Funcs {
		fmt.Fprintf(w, "%s;\n", csig(f))
		rt := make([]Type, len(f.Rets))
		for i := range f.Rets {
			rt[i] = f.Rets[i]
		}
		ftyp[f.Name] = rt
	}

	for _, g := range m.Globals {
		for i, s := range g.Name {
			t := g.Typs[i]
			gtyp[s] = t
			var u string
			switch v := g.Expr[i].(type) {
			case Cast:
				u = v.Arg.(Literal).Value
			case Literal:
				u = v.Value
			default:
				panic("global value must be (cast-to) literal")
			}
			fmt.Fprintf(w, "%s %s = %s;\n", ctype(t), s, u)
		}
	}
	for _, f := range m.Funcs {
		f.Exported = m.Exports[f.Name] || m.exportAll
		cfunc(w, f)
	}
	fmt.Fprintf(w, "void cinit(){\n")
	if m.Memory != "" {
		fmt.Fprintf(w, "_M=calloc(%s, 64*1024);\n", m.Memory)
	}
	if m.Memory2 != "" && MultiMemory {
		fmt.Fprintf(w, "_M2=calloc(%s, 64*1024);\n", m.Memory)
	}
	for _, d := range m.Data {
		fmt.Fprintf(w, "memcpy(_M+%d, %q, %d);\n", d.Off, d.Data, len(d.Data))
	}

	tmax := 0
	for _, e := range m.Table {
		if n := e.Off + len(e.Names); n > tmax {
			tmax = n
		}
	}
	fmt.Fprintf(w, "_F=(void *)malloc(%d*sizeof(void*));\n", tmax)
	for _, e := range m.Table {
		for i := range e.Names {
			fmt.Fprintf(w, "_F[%d]=%s;\n", e.Off+i, e.Names[i])
		}
	}
	fmt.Fprintf(w, "}\n")
	fmt.Fprintf(w, "int main(int *args, char **argv){cinit();return 0;}\n")
	out.Write(dumbindent.FormatBytes(nil, buf.Bytes(), &dumbindent.Options{Spaces: 1}))
}
func ctype(t Type) string {
	m := map[Type]string{
		V:     "void",
		I32:   "int32_t",
		U32:   "uint32_t",
		I64:   "int64_t",
		U64:   "uint64_t",
		F64:   "double",
		I8x16: "i8x16",
		I32x4: "i32x4",
		F64x2: "f64x2",
	}
	s, ok := m[t]
	if !ok {
		return string(t)
	}
	return s
}
func csig(f Func) (s string) {
	var r Type = V
	if len(f.Rets) == 1 {
		r = f.Rets[0]
	}
	s = fmt.Sprintf("%s %s(", ctype(r), cname(f.Name))
	for i := range f.Args {
		comma := ""
		if i > 0 {
			comma = ", "
		}
		s += comma + ctype(f.Args[i].Type) + " " + cname(f.Args[i].Name)
	}
	if len(f.Rets) > 1 {
		for i := range f.Rets {
			c := ", "
			if i == 0 && len(f.Args) == 0 {
				c = ""
			}
			s += fmt.Sprintf("%s%s* _R%d", c, ctype(f.Rets[i]), i)
		}
	}
	return s + ")"
}
func cname(s string) string { return strings.Replace(s, ".", "_", -1) }
func cfunc(w io.Writer, f Func) {
	ltyp = make(map[string]Type)
	for _, a := range f.Args {
		ltyp[a.Name] = a.Type
	}
	for _, l := range f.Locs {
		ltyp[l.Name] = l.Type
	}
	cstk = nil
	sig := csig(f)
	if f.Name == "main" {
		sig = "int main(int *args, char **argv)"
	}
	fmt.Fprintf(w, "%s{\n", sig)
	var buf bytes.Buffer
	for _, st := range f.Body {
		st.c(&buf)
	}
	m := make(map[Type][]string)
	for _, a := range f.Locs {
		t := a.Type
		m[t] = append(m[t], cname(a.Name))
	}
	for i, t := range cstk {
		m[t] = append(m[t], "_"+strconv.Itoa(i))
	}
	k := make([]string, 0)
	for t := range m {
		k = append(k, string(t))
	}
	sort.Strings(k)
	for _, s := range k {
		t := Type(s)
		fmt.Fprintf(w, "%s %s;\n", ctype(t), strings.Join(m[t], ","))
	}
	io.Copy(w, &buf)
	fmt.Fprintf(w, "}\n")
}

func c1() string        { return cn(0) }
func c2() string        { return cn(1) }
func c1n(t Type) string { cstk = append(cstk, t); return c1() }
func cn(n int) string   { return "_" + strconv.Itoa(len(cstk)-n-1) }
func (s Stmts) c(w io.Writer) {
	for _, st := range s {
		st.c(w)
	}
}
func (m Import) c(w io.Writer) {
	fmt.Fprintf(w, "IMPORT? %s, %s\n", m.Package, m.Func)
}
func (a Assign) c(w io.Writer) {
	for _, e := range a.Expr {
		e.c(w)
	}
	if len(a.Expr) == 0 { // "var t" assigns 0
		for _, tp := range a.Typs {
			t := ctype(tp)
			fmt.Fprintf(w, "%s=(%s)0;\n", c1n(tp), t)
		}
	}
	mod := a.Mod
	if mod == ":=" || mod == "" {
		mod = "="
	}
	for i, s := range a.Name {
		if s == "_" {
			continue
		}
		fmt.Fprintf(w, "%s %s%s;\n", cname(s), mod, cn(len(a.Name)-i-1))
	}
}
func (r Return) c(w io.Writer) {
	if len(r) == 1 {
		r[0].c(w)
		fmt.Fprintf(w, "return %s;\n", c1())
		return
	}
	if len(r) > 1 {
		for i, e := range r {
			e.c(w)
			fmt.Fprintf(w, "*_R%d=%s;\n", i, c1())
		}
	}
	fmt.Fprintf(w, "return;\n")
}
func (r Drop) c(w io.Writer) { r.Expr.c(w) }
func (n Nop) c(w io.Writer)  {}
func (l GlobalGet) c(w io.Writer) {
	t := gtyp[string(l)]
	fmt.Fprintf(w, "%s=%s;\n", c1n(t), l)
}
func (l GlobalGets) c(w io.Writer) {
	for _, li := range l {
		t := gtyp[string(li)]
		fmt.Fprintf(w, "%s=%s;\n", c1n(t), li)
	}
}
func (l LocalGet) c(w io.Writer) {
	t := ltyp[string(l)]
	fmt.Fprintf(w, "%s=%s;\n", c1n(t), cname(string(l)))
}
func (l LocalGets) c(w io.Writer) {
	for _, li := range l {
		t := ltyp[string(li)]
		fmt.Fprintf(w, "%s=%s;\n", c1n(t), cname(string(li)))
	}
}
func (u Unary) c(w io.Writer) {
	u.X.c(w)
	t := u.Op.Type
	fmt.Fprintf(w, "%s=%s%s;\n", c1n(t), u.Op.Name, c2())
}
func (b Binary) c(w io.Writer) {
	b.X.c(w)
	x := c1()
	b.Y.c(w)
	y := c1()
	t := b.Op.Type
	fmt.Fprintf(w, "%s=%s %s %s;\n", c1n(t), x, b.Op.Name, y)
}
func (l Literal) c(w io.Writer) { fmt.Fprintf(w, "%s = %s;\n", c1n(l.Type), l.Value) }
func ccall(w io.Writer, args []Expr, rt []Type) []string {
	a := make([]string, len(args))
	for i := range args {
		args[i].c(w)
		a[i] = c1()
	}
	if len(rt) > 1 {
		for _, t := range rt {
			a = append(a, "&"+c1n(t))
		}
	}
	if len(rt) == 1 {
		fmt.Fprintf(w, "%s=", c1n(rt[0]))
	}
	return a
}
func (c Call) c(w io.Writer) {
	switch c.Func {
	case "I8", "U8", "I16", "U16", "I32", "U32", "I64", "U64", "F32", "F64", "SetI8", "SetU8", "SetI16", "SetU16", "SetI32", "SetU32", "SetI64", "SetU64", "SetF32", "SetF64":
		c.Args[0].c(w)
		t := ""
		s := c.Func[len(c.Func)-2:]
		if s[1] == '8' {
			s = "8"
		}
		f, set := c.Func, false
		if strings.HasPrefix(f, "Set") {
			f, set = f[3:], true
		}
		if strings.HasPrefix(f, "I") {
			t = "int" + s + "_t"
		} else if strings.HasPrefix(f, "U") {
			t = "uint" + s + "_t"
		} else if c.Func == "F32" {
			t = "float"
		} else {
			t = "double"
		}
		if set {
			c.Args[1].c(w)
			fmt.Fprintf(w, "*((%s*)(_M+%s))=%s;\n", t, c2(), c1())
		} else {
			fmt.Fprintf(w, "%s=*((%s*)(_M+%s));\n", c1n(Type(t)), t, c2())
		}
	case "I32B": //bool2i32 (nop)
	case "panic":
		fmt.Fprintln(w, "PANIC?")
	default:
		args := ccall(w, c.Args, ftyp[c.Func])
		fmt.Fprintf(w, "%s(%s);\n", c.Func, strings.Join(args, ", "))
	}
	/*
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
	*/
}
func (c CallIndirect) c(w io.Writer) {
	c.Func.c(w)
	n := c1()
	args := ccall(w, c.Args, c.ResType)
	for i := range c.Args {
		c.Args[i].c(w)
	}
	r := "void"
	as := ""
	if len(c.ResType) == 1 {
		r = ctype(c.ResType[0])
		as = c1n(c.ResType[0]) + "="
	}
	a := make([]string, len(c.ArgType))
	for i, t := range c.ArgType {
		a[i] = ctype(t)
	}
	at := strings.Join(a, ",")
	// ((I(*)(I))MT[x0])
	// ((I(*)(I,I))MT[x0])
	fmt.Fprintf(w, "%s((%s(*)(%s))_F[%s])(%s);\n", as, r, at, n, strings.Join(args, ", "))
}
func (c Cast) c(w io.Writer) {
	c.Arg.c(w)
	t := c.Dst
	fmt.Fprintf(w, "%s=(%s)%s;\n", c1n(t), ctype(t), c2())
}
func (i If) c(w io.Writer) {
	i.If.c(w)
	fmt.Fprintf(w, "if(%s){\n", c1())
	i.Then.c(w)
	if i.Else != nil {
		fmt.Fprintf(w, "}else{\n")
		i.Else.c(w)
	}
	fmt.Fprintf(w, "}\n")
}
func (s Switch) c(w io.Writer) {
	s.E.c(w)
	fmt.Fprintf(w, "switch(%s){\n", c1())
	for i, c := range s.Case {
		fmt.Fprintf(w, "case %d:\n", i)
		c.c(w)
		fmt.Fprintf(w, "break;\n")
	}
	fmt.Fprintf(w, "default:\n")
	s.Def.c(w)
	fmt.Fprintf(w, "}\n")
}
func (f For) c(w io.Writer) {
	if f.Label != "" {
		fmt.Fprintf(w, "%s:\n", f.Label)
	}
	if f.Simple {
		fmt.Fprintf(w, "do{\n")
		f.Body.c(w)
		if f.Post != nil {
			f.Post.c(w)
		}
		if f.Cond != nil {
			f.Cond.c(w)
		}
		fmt.Fprintf(w, "}while(%s);\n", c1())
	} else {
		fmt.Fprintf(w, "while(1){\n")
		if f.Cond != nil {
			f.Cond.c(w)
		}
		fmt.Fprintf(w, "if(%s)break;\n", c1())
		f.Body.c(w)
		if f.Post != nil {
			f.Post.c(w)
		}
		fmt.Fprintf(w, "}\n")
	}
	if f.Label != "" {
		fmt.Fprintf(w, "%s_post:\n", f.Label)
	}
}
func (b Branch) c(w io.Writer) {
	if b.Label == "" {
		if b.Break {
			fmt.Fprintf(w, "break;\n")
		} else {
			fmt.Fprintf(w, "continue;\n")
		}
	} else {
		if b.Break {
			fmt.Fprintf(w, "goto %s_post;\n", b.Label)
		} else {
			fmt.Fprintf(w, "goto %s;\n", b.Label)
		}
	}
}

const chead string = `#include<stdio.h>
#include<stdint.h>
typedef int8_t  i8x16 __attribute__ ((vector_size (16)));
typedef int32_t i32x4 __attribute__ ((vector_size (16)));
typedef double  f64x2 __attribute__ ((vector_size (16)));
char *_M, *_M2;
void *_F;
`
