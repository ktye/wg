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
var gtyp map[string]Type
var ltyp map[string]Type
var ftyp map[string][]Type
var farg map[string]int
var cstk []Type

func (m Module) C(out io.Writer) {
	gtyp = make(map[string]Type)
	ftyp = make(map[string][]Type)
	farg = make(map[string]int)
	var buf bytes.Buffer
	w := &buf
	w.Write([]byte(chead))

	for _, f := range m.Funcs {
		if f.Name == "main" {
			continue
		}
		fmt.Fprintf(w, "%s;\n", csig(f))
		rt := make([]Type, len(f.Rets))
		for i := range f.Rets {
			rt[i] = f.Rets[i]
		}
		ftyp[f.Name] = rt
		farg[f.Name] = len(f.Args)
	}
	// register return type for some built-in functions
	for _, s := range []string{"Memorycopy", "Memorycopy2", "Memorycopy3", "Memoryfill"} {
		ftyp[s] = []Type{}
	}
	for _, s := range []string{"Memorygrow", "Memorygrow2", "I32clz", "I64popcnt"} {
		ftyp[s] = []Type{I32}
	}
	for _, s := range []string{"F64abs", "F64sqrt", "F64copysign", "F64min", "F64max", "F64floor"} {
		ftyp[s] = []Type{F64}
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
	fmt.Fprintf(w, "void cinit(){\n_memorysize=1;\n_memorysize2=1;\n")
	if m.Memory != "" {
		fmt.Fprintf(w, "_M=calloc(%s, 64*1024);\n", m.Memory)
	}
	if m.Memory2 != "" && MultiMemory {
		fmt.Fprintf(w, "_M2=calloc(%s, 64*1024);\n", m.Memory)
	}
	for _, d := range m.Data {
		q := cquote(d.Data)
		fmt.Fprintf(w, "memcpy(_M+%d, \"%s\", %d);\n", d.Off, q, len(d.Data))
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
	w.Write([]byte(ctail))
	out.Write(dumbindent.FormatBytes(nil, buf.Bytes(), &dumbindent.Options{Spaces: 1}))
}
func cquote(s string) string {
	var w strings.Builder
	for _, c := range s {
		switch c {
		case '\t':
			w.Write([]byte(`\t`))
		case '\n':
			w.Write([]byte(`\n`))
		case '\\':
			w.Write([]byte(`\\`))
		case '\'':
			w.Write([]byte(`\'`))
		case '"':
			w.Write([]byte(`\"`))
		default:
			if c < 32 || c > 127 {
				fmt.Fprintf(&w, "\\%o", c)
			} else {
				w.Write([]byte{byte(c)})
			}
		}
	}
	return w.String()
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
	if f.Name == "main" {
		f.Name = "main_"
	}
	sig := csig(f)
	fmt.Fprintf(w, "%s{\n", sig)
	var buf bytes.Buffer
	for _, st := range f.Body {
		st.c(&buf)
	}
	m := make(map[string][]string)
	for _, a := range f.Locs {
		t := ctype(a.Type)
		m[t] = append(m[t], cname(a.Name))
	}
	for i, t := range cstk {
		m[ctype(t)] = append(m[ctype(t)], "_"+strconv.Itoa(i))
	}

	k := []string{"int8_t", "uint8_t", "int16_t", "uint16_t", "int32_t", "uint32_t", "int64_t", "uint64_t", "double", "i8x16", "i32x4", "f64x2"}
	nk := 0
	for _, t := range k {
		v := m[t]
		if len(v) > 0 {
			fmt.Fprintf(w, "%s %s;\n", t, strings.Join(v, ","))
			nk++
		}
	}
	if nk != len(m) {
		var t []string
		for k := range m {
			t = append(t, k)
		}
		panic("there are unknown local types: " + strings.Join(t, "|"))
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
	// fmt.Fprintf(w, "// assign %s\n", a.Mod)
	// defer func() { fmt.Fprintf(w, "//<assign\n") }()
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
	} else if mod == "&^=" {
		x := cname(a.Name[0])
		fmt.Fprintf(w, "%s=%s&~%s;\n", x, x, c1())
		return
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
	if b.Op.Name == "&^" {
		b.Op.Name = "&~"
	}
	b.X.c(w)
	x := c1()
	b.Y.c(w)
	y := c1()
	t := b.Op.Type
	fmt.Fprintf(w, "%s=%s %s %s;\n", c1n(t), x, b.Op.Name, y)
}
func (l Literal) c(w io.Writer) {
	fmt.Fprintf(w, "%s = %s;\n", c1n(l.Type), l.Value)
}
func ccall(w io.Writer, f string, args []Expr, rt []Type) []string {
	var a []string
	if n, ok := farg[f]; ok && len(args) == 1 && n > 1 { // chain f(g(..)), f:multi-arg, g:multi-ret
		args[0].c(w)
		for i := 0; i < n; i++ {
			a = append(a, cn(i))
		}
	} else {
		for i := range args {
			args[i].c(w)
			a = append(a, c1())
		}
	}
	if len(rt) > 1 {
		for _, t := range rt {
			a = append(a, "&"+c1n(t))
		}
	}
	return a
}
func (c Call) c(w io.Writer) {
	// fmt.Fprintf(w, "//call %s\n", c.Func)
	// defer func() { fmt.Fprintf(w, " // <%s\n", c.Func) }()
	call := func(f string) {
		//if f == "I32clz" {
		//	fmt.Fprintf(w, "// rt: %d\n", len(rt))
		//}
		rt, ok := ftyp[c.Func]
		if !ok {
			panic(string(c.Func) + " is not defined")
		}
		args := ccall(w, c.Func, c.Args, rt)
		if len(rt) == 1 {
			fmt.Fprintf(w, "%s=", c1n(rt[0]))
		}
		fmt.Fprintf(w, "%s(%s);\n", f, strings.Join(args, ", "))
	}
	args2 := func() (x, y string) {
		c.Args[0].c(w)
		x = c1()
		c.Args[1].c(w)
		y = c1()
		return x, y
	}
	switch c.Func {
	case "I8", "U8", "I16", "U16", "I32", "U32", "I64", "U64", "F32", "F64", "SetI8", "SetU8", "SetI16", "SetU16", "SetI32", "SetU32", "SetI64", "SetU64", "SetF32", "SetF64":
		c.Args[0].c(w)
		a := c1()
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
			fmt.Fprintf(w, "*((%s*)(_M+%s))=%s;\n", t, a, c1())
		} else {
			fmt.Fprintf(w, "%s=*((%s*)(_M+%s));\n", c1n(Type(t)), t, a)
		}
	case "I32B": //bool2i32 (nop)
		ccall(w, c.Func, c.Args, ftyp[c.Func])
	case "I8x16load", "I32x4load", "F64x2load": // others?
		c.Args[0].c(w)
		t := Type(c.Func[:5])
		fmt.Fprintf(w, "memcpy(&%s, _M+%s, 16);\n", c1n(t), c2())
	case "I8x16store", "I32x4store", "F64x2store":
		x, y := args2()
		fmt.Fprintf(w, "memcpy(_M+%s, &%s, 16);\n", x, y)
	case "I8x16splat", "I32x4splat", "F64x2splat":
		a, b, t := "", "i32x4", I32x4
		if c.Func == "I8x16splat" {
			a, b, t = "(int8_t)", "i8x16", I8x16
		} else if c.Func == "F64x2splat" {
			a, b, t = "(double)", "f64x2", F64x2
		}
		c.Args[0].c(w)
		x := c1()
		fmt.Fprintf(w, "%s=%s%s-(%s){}; //broadcast\n", c1n(Type(t)), a, x, b)
	case "I8x16.Eq", "I32x4.Eq", "I8x16.And", "I32x4.And":
		t := Type(c.Func[:5])
		suffix := c.Func[6:]
		a := ccall(w, "", c.Args, []Type{t})
		m := map[string]string{
			"Eq":  "==",
			"And": "&",
		}
		op, ok := m[suffix] //"=="
		if ok == false {
			panic("simd op does not exist: " + suffix)
		}
		fmt.Fprintf(w, "%s=%s %s %s;\n", c1n(t), a[0], op, a[1])
	case "I8x16.Not", "I32x4.Not", "I8x16.Neg", "I32x4.Neg", "F64x2.Neg":
		t := Type(c.Func[:5])
		suffix := c.Func[6:]
		a := ccall(w, "", c.Args, []Type{t})
		op := "~"
		if suffix == "Neg" {
			op = "-"
		} else if suffix != "Not" {
			panic("unknown simd op: " + suffix)
		}
		fmt.Fprintf(w, "%s=%s%s;\n", c1n(t), op, a[0])
	case "I8x16.Add", "I32x4.Add", "F64x2.Add", "I8x16.Sub", "I32x4.Sub", "F64x2.Sub", "I32x4.Mul", "F64x2.Mul", "F64x2.Div", "I8x16.Lt_s", "I8x16.Gt_s":
		t := c.Func[:5]
		op := c.Func[6:]
		ops := map[string]string{
			"Add":  "+",
			"Sub":  "-",
			"Mul":  "*",
			"Div":  "/",
			"Lt_s": "<",
			"Gt_s": ">",
		}
		op = ops[op]
		x, y := args2()
		fmt.Fprintf(w, "%s=%s%s%s;\n", c1n(Type(t)), x, op, y)
	case "I32x4.Shr_s":
		x, y := args2()
		fmt.Fprintf(w, "%s=%s>>%s;\n", c1n("i32x4"), x, y)
	case "I8x16.Min_s", "I32x4.Min_s", "F64x2.Pmin", "I8x16.Max_s", "I32x4.Max_s", "F64x2.Pmax":
		t := strings.ToLower(c.Func[:5])
		op := strings.ToLower(c.Func[6:])
		x, y := args2()
		fmt.Fprintf(w, "%s%s(%s,%s,%s);\n", t, op, c1n(Type(t)), x, y)
	case "I8x16.All_true", "I32x4.All_true", "I8x16.Any_true", "I32x4.Any_true":
		n := 4
		if strings.HasPrefix(c.Func, "I8x16") {
			n = 16
		}
		ccall(w, "", c.Args, []Type{I32})
		v := c1()
		s := ""
		for i := 0; i < n; i++ {
			op := ""
			if i > 0 {
				op = " && "
				if strings.HasSuffix(c.Func, "Any_true") {
					op = " || "
				}
			}
			s += op + v + "[" + strconv.Itoa(i) + "]"
		}
		fmt.Fprintf(w, "%s=(int32_t)(%s);\n", c1n("int32_t"), s)
	case "F64x2.Replace_lane1":
		x, y := args2()
		z := c1n("f64x2")
		fmt.Fprintf(w, "%s=%s;%s[1]=%s; //replace_lane1\n", z, x, z, y)
	case "I8x16.Extract_lane_s0":
		c.Args[0].c(w)
		fmt.Fprintf(w, "%s=%s[0]; //I8x16.Extract_lane_s0\n", c1n("int32_t"), c2())
	case "I8x16.Abs", "I32x4.Abs":
		t := strings.ToLower(c.Func[:5])
		c.Args[0].c(w)
		fmt.Fprintf(w, "%sabs(%s,%s);\n", t, c1n(Type(t)), c1()) // or: _mm_abs_epi8
	case "F64x2.Abs", "F64x2.Sqrt", "F64.Floor":
		op := strings.ToLower(c.Func[6:])
		c.Args[0].c(w)
		y := c1()
		x := c1n("f64x2")
		fmt.Fprintf(w, "%s[0]=F64%s(%s[0]);%s[1]=F64%s(%s[1]);\n", x, op, y, x, op, y)
	case "F64reinterpret_i64":
		c.Args[0].c(w)
		fmt.Fprintf(w, "%s=*(double*)&%s;\n", c1n("double"), c1())
	case "I64reinterpret_f64":
		c.Args[0].c(w)
		fmt.Fprintf(w, "%s=*(uint64_t*)&%s;\n", c1n("uint64_t"), c1())
	case "panic":
		ccall(w, "", c.Args, []Type{I32})
		fmt.Fprintln(w, "exit(1); // todo: trap\n")
	case "Memorysize":
		fmt.Fprintf(w, "%s=_memorysize;\n", c1n("int32_t"))
	case "Memorysize2":
		fmt.Fprintf(w, "%s=_memorysize2;\n", c1n("int32_t"))
	case "wasi_unstable.proc_exit":
		c.Args[0].c(w)
		fmt.Fprintf(w, "exit(%s);\n", c1())
	case "wasi_unstable.clock_time_get":
		x, y := args2()
		c.Args[2].c(w)
		z := c1()
		fmt.Fprintf(w, "%s=wasi_clock_time_get(%s,%s,%s);\n", c1n("uint64_t"), x, y, z)
	case "wasi_unstable.fd_read", "wasi_unstable.fd_write", "wasi_unstable.args_sizes_get", "wasi_unstable.args_get", "wasi_unstable.path_open", "wasi_unstable.fd_seek", "wasi_unstable.fd_close":
		f := c.Func[14:]
		var a []string
		for i := range c.Args {
			c.Args[i].c(w)
			a = append(a, c1())
		}
		fmt.Fprintf(w, "%s=wasi_%s(%s);", c1n("int32_t"), f, strings.Join(a, ","))
	default:
		// nyi: "I64clz", "I32ctz", "I64ctz", "I32popcnt", "I64popcnt", "F64abs", "F64sqrt", "F64ceil", "F64floor", "F64nearest", "F64min", "F64max", "F64copysign", "I64reinterpret_f64", "F64reinterpret_i64", "I32reinterpret_f32", "F32reinterpret_i32"
		call(c.Func)
	}
}
func (c CallIndirect) c(w io.Writer) {
	c.Func.c(w)
	n := c1()
	args := ccall(w, "", c.Args, c.ResType)
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
	if len(c.ResType) > 1 {
		for _, t := range c.ResType {
			a = append(a, ctype(t)+"*")
		}
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
	if len(s.Def) > 0 {
		fmt.Fprintf(w, "default:\n")
		s.Def.c(w)
	}
	fmt.Fprintf(w, "}\n")
}
func (f For) c(w io.Writer) {
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
			fmt.Fprintf(w, "if(%s)break;\n", c1())
		}
		f.Body.c(w)
		if f.Post != nil {
			f.Post.c(w)
		}
		fmt.Fprintf(w, "}\n")
	}
	if f.Label != "" {
		fmt.Fprintf(w, "%s:\n", f.Label)
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
		if b.Break == false {
			panic("break (jump-backwards)")
		}
		fmt.Fprintf(w, "goto %s;\n", b.Label)
	}
}
func (p Printf) c(w io.Writer) {
	fmt.Fprintf(w, "printf(%s", p.Format)
	for _, a := range p.Args {
		fmt.Fprintf(w, ",%s", a)
	}
	fmt.Fprintf(w, ");fflush(stdout);\n")
}

const chead string = `#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>
#include<string.h>
#include<time.h>
typedef int8_t  i8x16 __attribute__ ((vector_size (16)));
typedef int32_t i32x4 __attribute__ ((vector_size (16)));
typedef double  f64x2 __attribute__ ((vector_size (16)));
#define F64abs        __builtin_fabs
#define F64sqrt       __builtin_sqrt
#define F64floor      __builtin_floor
#define F64min        __builtin_fmin
#define F64max        __builtin_fmax
#define F64copysign   __builtin_copysign
char *_M, *_M2;
void **_F;
int32_t _memorysize, _memorysize2;
FILE *_fd_;
int    args_;
char **argv_;
int32_t Memorygrow(int32_t delta){
 int32_t r=_memorysize;
 _memorysize+=delta;
 _M=(char *)realloc(_M, 64*1024*_memorysize);
 return r;
}
int32_t Memorygrow2(int32_t delta){
 int32_t r=_memorysize2;
 _memorysize2+=delta;
 _M2=(char *)realloc(_M2, 64*1024*_memorysize2);
 return r;
}
void Memorycopy (int32_t dst, int32_t src, int32_t n){ memcpy(_M +dst, _M +src, n); }
void Memorycopy2(int32_t dst, int32_t src, int32_t n){ memcpy(_M2+dst, _M2+src, n); }
void Memorycopy3(int32_t dst, int32_t src, int32_t n){ memcpy( _M+dst, _M2+src, n); }
void Memoryfill(int32_t p, int32_t v, int32_t n){ memset(_M+p, (int)v, (size_t)n); }
int32_t I32clz(uint32_t x) { return (int32_t)__builtin_clz((unsigned int)x); }
int32_t I64popcnt(uint64_t x){ return (int32_t)__builtin_popcountl(x); }
void i8x16abs(i8x16 dst, i8x16 src){
 dst[0]=(src[0]<0)?-src[0]:src[0];
 dst[1]=(src[1]<0)?-src[1]:src[1];
 dst[2]=(src[2]<0)?-src[2]:src[2];
 dst[3]=(src[3]<0)?-src[3]:src[3];
 dst[4]=(src[4]<0)?-src[4]:src[4];
 dst[5]=(src[5]<0)?-src[5]:src[5];
 dst[6]=(src[6]<0)?-src[6]:src[6];
 dst[7]=(src[7]<0)?-src[7]:src[7];
 dst[8]=(src[8]<0)?-src[8]:src[8];
 dst[9]=(src[9]<0)?-src[9]:src[9];
 dst[10]=(src[10]<0)?-src[10]:src[10];
 dst[11]=(src[11]<0)?-src[11]:src[11];
 dst[12]=(src[12]<0)?-src[12]:src[12];
 dst[13]=(src[13]<0)?-src[13]:src[13];
 dst[14]=(src[14]<0)?-src[14]:src[14];
 dst[15]=(src[15]<0)?-src[15]:src[15];
}
void i32x4abs(i32x4 dst, i32x4 src){
 dst[0]=(src[0]<0)?-src[0]:src[0];
 dst[1]=(src[1]<0)?-src[1]:src[1];
 dst[2]=(src[2]<0)?-src[2]:src[2];
 dst[3]=(src[3]<0)?-src[3]:src[3];
}
void i8x16min_s(i8x16 r, i8x16 x, i8x16 y){
 r[ 0]=(x[ 0]<y[ 0])?x[ 0]:y[ 0];
 r[ 1]=(x[ 1]<y[ 1])?x[ 1]:y[ 1];
 r[ 2]=(x[ 2]<y[ 2])?x[ 2]:y[ 2];
 r[ 3]=(x[ 3]<y[ 3])?x[ 3]:y[ 3];
 r[ 4]=(x[ 4]<y[ 4])?x[ 4]:y[ 4];
 r[ 5]=(x[ 5]<y[ 5])?x[ 5]:y[ 5];
 r[ 6]=(x[ 6]<y[ 6])?x[ 6]:y[ 6];
 r[ 7]=(x[ 7]<y[ 7])?x[ 7]:y[ 7];
 r[ 8]=(x[ 8]<y[ 8])?x[ 8]:y[ 8];
 r[ 9]=(x[ 9]<y[ 9])?x[ 9]:y[ 9];
 r[10]=(x[10]<y[10])?x[10]:y[10];
 r[11]=(x[11]<y[11])?x[11]:y[11];
 r[12]=(x[12]<y[12])?x[12]:y[12];
 r[13]=(x[13]<y[13])?x[13]:y[13];
 r[14]=(x[14]<y[14])?x[14]:y[14];
 r[15]=(x[15]<y[15])?x[15]:y[15];
}
void i32x4min_s(i32x4 r, i32x4 x, i32x4 y){
 r[ 0]=(x[ 0]<y[ 0])?x[ 0]:y[ 0];
 r[ 1]=(x[ 1]<y[ 1])?x[ 1]:y[ 1];
 r[ 2]=(x[ 2]<y[ 2])?x[ 2]:y[ 2];
 r[ 3]=(x[ 3]<y[ 3])?x[ 3]:y[ 3];
}
void f64x2pmin(f64x2 r, f64x2 x, f64x2 y){
 r[ 0]=(x[ 0]<y[ 0])?x[ 0]:y[ 0];
 r[ 1]=(x[ 1]<y[ 1])?x[ 1]:y[ 1];
}
void i8x16max_s(i8x16 r, i8x16 x, i8x16 y){
 r[ 0]=(x[ 0]>y[ 0])?x[ 0]:y[ 0];
 r[ 1]=(x[ 1]>y[ 1])?x[ 1]:y[ 1];
 r[ 2]=(x[ 2]>y[ 2])?x[ 2]:y[ 2];
 r[ 3]=(x[ 3]>y[ 3])?x[ 3]:y[ 3];
 r[ 4]=(x[ 4]>y[ 4])?x[ 4]:y[ 4];
 r[ 5]=(x[ 5]>y[ 5])?x[ 5]:y[ 5];
 r[ 6]=(x[ 6]>y[ 6])?x[ 6]:y[ 6];
 r[ 7]=(x[ 7]>y[ 7])?x[ 7]:y[ 7];
 r[ 8]=(x[ 8]>y[ 8])?x[ 8]:y[ 8];
 r[ 9]=(x[ 9]>y[ 9])?x[ 9]:y[ 9];
 r[10]=(x[10]>y[10])?x[10]:y[10];
 r[11]=(x[11]>y[11])?x[11]:y[11];
 r[12]=(x[12]>y[12])?x[12]:y[12];
 r[13]=(x[13]>y[13])?x[13]:y[13];
 r[14]=(x[14]>y[14])?x[14]:y[14];
 r[15]=(x[15]>y[15])?x[15]:y[15];
}
void i32x4max_s(i32x4 r, i32x4 x, i32x4 y){
 r[ 0]=(x[ 0]>y[ 0])?x[ 0]:y[ 0];
 r[ 1]=(x[ 1]>y[ 1])?x[ 1]:y[ 1];
 r[ 2]=(x[ 2]>y[ 2])?x[ 2]:y[ 2];
 r[ 3]=(x[ 3]>y[ 3])?x[ 3]:y[ 3];
}
void f64x2pmax(f64x2 r, f64x2 x, f64x2 y){
 r[ 0]=(x[ 0]>y[ 0])?x[ 0]:y[ 0];
 r[ 1]=(x[ 1]>y[ 1])?x[ 1]:y[ 1];
}
uint64_t wasi_clock_time_get(int32_t id, int32_t prec, int32_t res){
 struct timespec t;
 clock_gettime((clockid_t)id, &t);
 return (uint64_t)(1000000000L)*(uint64_t)(t.tv_sec) + (uint64_t)t.tv_nsec;
}
int32_t wasi_fd_read(int32_t fd, int32_t p, int32_t nio, int32_t wa){
 FILE  *fp=stdin;
 int32_t a=*((int32_t*)(_M+p));
 int32_t n=*((int32_t*)(_M+p+4));
 if(fd!=0)fp=_fd_;
 int32_t w=(int32_t)fread(_M+a, 1, (size_t)n, fp);
 *((int32_t*)(_M+wa))=w;
 return w<0;
}
int32_t wasi_fd_write(int32_t fd, int32_t p, int32_t nio, int32_t wa){
 FILE  *fp=stdout;
 int32_t a=*((int32_t*)(_M+p));
 int32_t n=*((int32_t*)(_M+p+4));
 if(fd!=1)fp=_fd_;
 if(fd==2)fp=stderr;
 int32_t w=(int32_t)fwrite(_M+a, 1, (size_t)n, fp);
 *((int32_t*)(_M+wa))=w;
 return w<0;
}
int32_t wasi_args_sizes_get(int32_t np, int32_t sp){
 int32_t n=0;
 for(int i=0;i<args_;i++)n+=1+(int32_t)strlen(argv_[i]);
 *((int32_t*)(_M+np))=(int32_t)args_;
 *((int32_t*)(_M+sp))=n;
}
int32_t wasi_args_get(int32_t p, int32_t sp){
 char *o=_M+sp;
 for(int i=0;i<args_;i++){
  size_t n=strlen(argv_[i]);
  memcpy(o,argv_[i],1+n);
  o+=1+n;
 }
 return 0;
}
int32_t wasi_path_open(int32_t fd, int32_t dirflags, int32_t path, int32_t pathlen, int32_t oflags, int64_t baserights, int64_t inheritrights, int32_t fdflags, int32_t newfp){
 char *name=malloc(1+pathlen);
 memcpy(name,_M+path,1+pathlen);
 name[pathlen]=0;
 if(oflags==0) _fd_=fopen(name,"r");//assume read
 else          printf("todo: write to %s\n", name);  //_fd_=fopen(name,"w");  //assume write
 free(name);
 return 0;
}
int32_t wasi_fd_seek(int32_t fp, int64_t offset, int32_t whence, int32_t rp){
 *((int32_t*)(_M+rp))=(int32_t)fseek(_fd_, (long int)offset, (int)whence);
 return 0;
}
int32_t wasi_fd_close(int32_t fp){ fclose(_fd_); return 0; }
`
const ctail string = `int main(int args, char **argv){
 args_=(int32_t)args;
 argv_=argv;
 cinit();
 main_();
 return 0;
}`
