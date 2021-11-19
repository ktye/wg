package wg

import (
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"

	"github.com/ktye/wg/dumbindent"
)

// convert wg ast to C
var ctop int
var gtyp map[string]Type
var ltyp map[string]Type
var ftyp map[string][]Type

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
		fmt.Fprintf(w, "memcpy(_M+%d, %q, %d);", d.Off, d.Data, len(d.Data))
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
	ctop = 0
	sig := csig(f)
	if f.Name == "main" {
		sig = "int main(int *args, char **argv)"
	}
	fmt.Fprintf(w, "%s{\n", sig)
	for _, a := range f.Locs {
		fmt.Fprintf(w, "%s %s;\n", ctype(a.Type), cname(a.Name))
	}
	for _, st := range f.Body {
		st.c(w)
	}
	fmt.Fprintf(w, "}\n")
}

func c1() string      { return cn(0) }
func c2() string      { return cn(1) }
func c1n() string     { ctop++; return c1() }
func cn(n int) string { return "_" + strconv.Itoa(ctop-n) }
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
	mod := a.Mod
	if mod == ":=" || mod == "" {
		mod = "="
	}
	for i, s := range a.Name {
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
func (r Drop) c(w io.Writer) {
	r.Expr.c(w)
	fmt.Fprintf(w, "//drop\n")
}
func (n Nop) c(w io.Writer)       {}
func (l GlobalGet) c(w io.Writer) { fmt.Fprintf(w, "%s %s=%s;\n", ctype(gtyp[string(l)]), c1n(), l) }
func (l GlobalGets) c(w io.Writer) {
	for _, li := range l {
		fmt.Fprintf(w, "%s %s=%s;\n", ctype(gtyp[li]), c1n(), li)
	}
}
func (l LocalGet) c(w io.Writer) {
	fmt.Fprintf(w, "%s %s=%s;\n", ctype(ltyp[string(l)]), c1n(), cname(string(l)))
}
func (l LocalGets) c(w io.Writer) {
	for _, li := range l {
		fmt.Fprintf(w, "%s %s=%s;\n", ctype(ltyp[li]), c1n(), cname(string(li)))
	}
}
func (u Unary) c(w io.Writer) {
	u.X.c(w)
	fmt.Fprintf(w, "%s %s=%s%s;\n", ctype(u.Op.Type), c1n(), u.Op.Name, c2())
}
func (b Binary) c(w io.Writer) {
	b.X.c(w)
	x := c1()
	b.Y.c(w)
	y := c1()
	fmt.Fprintf(w, "%s %s=%s %s %s;\n", ctype(b.Op.Type), c1n(), x, b.Op.Name, y)
}
func (l Literal) c(w io.Writer) { fmt.Fprintf(w, "%s %s = %s;\n", ctype(l.Type), c1n(), l.Value) }
func ccall(w io.Writer, args []Expr, rt []Type) []string {
	a := make([]string, len(args))
	for i := range args {
		args[i].c(w)
		a[i] = c1()
	}
	if len(rt) > 1 {
		for _, t := range rt {
			fmt.Fprintf(w, "%s %s;\n", ctype(t), c1n())
			a = append(a, "&"+c1())
		}
	}
	if len(rt) == 1 {
		fmt.Fprintf(w, "%s %s=", ctype(rt[0]), c1n())
	}
	return a
}
func (c Call) c(w io.Writer) {
	if c.Func == "panic" {
		fmt.Fprintln(w, "PANIC?")
		return
	}
	args := ccall(w, c.Args, ftyp[c.Func])
	fmt.Fprintf(w, "%s(%s);\n", c.Func, strings.Join(args, ", "))
}
func (c CallIndirect) c(w io.Writer) {
	c.Func.c(w)
	n := c1()
	args := ccall(w, c.Args, c.ResType)
	for i := range c.Args {
		c.Args[i].c(w)
	}
	r := ""
	if len(c.ResType) == 1 {
		r = ctype(c.ResType[0])
	}
	a := make([]string, len(c.ArgType))
	for i, t := range c.ArgType {
		a[i] = ctype(t)
	}
	at := strings.Join(a, ",")
	// ((I(*)(I))MT[x0])
	// ((I(*)(I,I))MT[x0])
	fmt.Fprintf(w, "((%s(*)(%s))_F[%s])(%s);\n", r, at, n, strings.Join(args, ", "))
}
func (c Cast) c(w io.Writer) {
	c.Arg.c(w)
	d := ctype(c.Dst)
	fmt.Fprintf(w, "%s %s=(%s)%s;\n", d, c1n(), d, c2())
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
