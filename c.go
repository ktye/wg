package wg

import (
	"bytes"
	"encoding/hex"
	"fmt"
	"io"
	"strconv"
	"strings"
)

func (m Module) C(ww io.Writer) {
	w := cfmt{b: bytes.NewBuffer(nil), w: ww}

	maxfn := 0
	for _, t := range m.Table {
		if n := t.Off + len(t.Names); n > maxfn {
			maxfn = n
		}
	}

	typ := func(t Type) string {
		return string(t)
	}
	fname := func(s string) string {
		if strings.Index("  main ldexp hypot atan2 atan exp log pow ", " "+s+" ") > 0 {
			return s + "_"
		}
		return s
	}
	opa := func(s string) string {
		if s == "&^" {
			return "&~"
		}
		return s
	}

	var ex func(e Expr)
	decl := make(map[string]bool)
	var locl map[string]string
	rename := func(s string) string {
		if n, o := locl[s]; o {
			return n
		}
		return s
	}

	em := func(e Expr) {
		switch e.(type) {
		case LocalGet, LocalGets, GlobalGet, GlobalGets, Call, CallIndirect, Literal:
			ex(e)
		default:
			fmt.Fprintf(w, "(")
			ex(e)
			fmt.Fprintf(w, ")")
		}
	}
	simdcall := func(c Call) bool {
		if c.Func == "Iota" {
			fmt.Fprintf(w, "{0,1,2,3,4,5,6,7};")
			return true
		}
		if c.Func == "VI1" {
			fmt.Fprintf(w, "{1,1,1,1,1,1,1,1};")
			return true
		}
		if c.Func == "VIsplat" {
			em(c.Args[0])
			return true
		}
		d := map[string]string{
			"VI.Add": "+", "VF.Add": "+",
			"VI.Sub": "-", "VF.Sub": "-",
			"VI.Mul": "*", "VF.Mul": "*",
			"VI.And":  "&",
			"VI.Lt_s": "<", "VI.Gt_s": ">", "VI.Eq": "==",
		}
		if s, o := d[c.Func]; o {
			fmt.Fprintf(w, "(")
			em(c.Args[0])
			fmt.Fprintf(w, "%s", s)
			em(c.Args[1])
			fmt.Fprintf(w, ")")
			return true
		}
		if c.Func == "VF.Hsum" {
			if l, o := c.Args[0].(LocalGets); o {
				s := locl[l[0]]
				fmt.Fprintf(w, "%s[0]+%s[1]+%s[2]+%s[3];", s, s, s, s)
			} else {
				panic("VF.Hsum expects localgets")
			}
			return true
		}
		m := map[string]string{
			"VI.Min_s":  "__builtin_elementwise_min",
			"VF.Pmin":   "__builtin_elementwise_min",
			"VI.Abs":    "__builtin_elementwise_abs",
			"VF.Abs":    "__builtin_elementwise_abs",
			"VI.Max_s":  "__builtin_elementwise_max",
			"VF.Pmax":   "__builtin_elementwise_max",
			"VI.Hmin_s": "__builtin_reduce_min",
			"VI.Hmax_s": "__builtin_reduce_min",
			"VI.Hsum":   "__builtin_reduce_add",
			"VF.Hmin":   "__builtin_reduce_min",
			"VF.Hmax":   "__builtin_reduce_max",
			"VI.Neg":    "-", "VF.Neg": "-",
			"VF.Sqrt": "VFsqrt",
		}
		if s, o := m[c.Func]; o {
			c.Func = s
			ex(c)
			return true
		}
		return false
	}
	splats := func(v Expr, n int) bool {
		fmt.Fprintf(w, "{")
		for i := 0; i < n; i++ {
			if i > 0 {
				fmt.Fprintf(w, ",")
			}
			ex(v)
		}
		fmt.Fprintf(w, "};")
		return true
	}
	splat := func(v Expr) bool {
		if c, o := v.(Call); o {
			if c.Func == "VIsplat" {
				return splats(c.Args[0], 8)
			} else if c.Func == "VFsplat" {
				return splats(c.Args[0], 4)
			}
		}
		return false
	}
	selfassign := func(s string, e Expr) bool {
		if l, o := e.(LocalGets); o && len(l) == 1 && l[0] == s {
			return true
		}
		return false
	}

	ex = func(e Expr) {
		switch v := e.(type) {
		case Assign:
			for i := range v.Name {
				mod := strings.TrimSuffix(v.Mod, "=")
				if mod == ":" {
					mod = ""
				}
				if mod == "" && len(v.Expr) == len(v.Name) && selfassign(v.Name[i], v.Expr[i]) {
					continue
				}
				ant, tna := mod == "&^", ""
				if ant { // &^= => &= ^..
					mod, tna = "&", "~"
				}
				if decl[v.Name[i]] == false {
					decl[v.Name[i]] = true
					fmt.Fprintf(w, "%s ", typ(v.Typs[i]))
				}
				fmt.Fprintf(w, "%s %s= %s", rename(v.Name[i]), mod, tna)
				if len(v.Expr) < len(v.Name) {
					fmt.Fprintf(w, "0")
				} else {
					if tna != "" {
						em(v.Expr[i])
					} else if splat(v.Expr[i]) {
					} else {
						ex(v.Expr[i])
					}
				}
				fmt.Fprintf(w, ";\n")
			}
		case Binary:
			em(v.X)
			fmt.Fprintf(w, "%s", opa(v.Op.Name))
			em(v.Y)
		case Branch:
			s := ""
			if v.Label != "" {
				s = "/*" + s + "*/"
			}
			if v.Break {
				fmt.Fprintf(w, "break%s;\n", s)
			} else {
				fmt.Fprintf(w, "continue%s;\n", s)
			}
		case Call:
			if simdcall(v) {
				break
			}
			fmt.Fprintf(w, "%s(", fname(v.Func))
			for i := range v.Args {
				if i > 0 {
					fmt.Fprintf(w, ", ")
				}
				ex(v.Args[i])
			}
			fmt.Fprintf(w, ")")
		case CallIndirect:
			var at []string
			rt := "void"
			if len(v.ResType) > 0 {
				rt = typ(v.ResType[0])
			}
			for _, a := range v.ArgType {
				at = append(at, typ(a))
			}
			if len(at) == 0 {
				at = []string{"void"}
			}
			fmt.Fprintf(w, "((%s(*)(%s))$fn[", rt, strings.Join(at, ","))
			ex(v.Func)
			fmt.Fprintf(w, "])(")
			for i := range v.Args {
				if i > 0 {
					fmt.Fprintf(w, ", ")
				}
				ex(v.Args[i])
			}
			fmt.Fprintf(w, ")")
		case Cast:
			fmt.Fprintf(w, "(%s)", typ(v.Dst))
			em(v.Arg)
		case Drop:
			em(v.Expr)
			fmt.Fprintf(w, ";\n")
		case For:
			if v.Simple {
				fmt.Fprintf(w, "do{\n")
				ex(v.Body[:len(v.Body)-1]) // strip continue
				if v.Post != nil {
					ex(v.Post)
					fmt.Fprintf(w, ";\n")
				}
				fmt.Fprintf(w, "}while(")
				ex(v.Cond)
				fmt.Fprintf(w, ");\n")
			} else {
				fmt.Fprintf(w, "while(")
				if v.Cond == nil {
					fmt.Fprintf(w, "1")
				} else {
					ex(v.Cond)
				}
				fmt.Fprintf(w, "){\n")
				ex(v.Body)
				if v.Post != nil {
					ex(v.Post)
					fmt.Fprintf(w, ";\n")
				}
				fmt.Fprintf(w, "}")
			}
		case GlobalGet:
			fmt.Fprintf(w, "%s", v)
		case GlobalGets:
			if len(v) != 1 {
				panic("1!=#GlobalGets")
			}
			fmt.Fprintf(w, "%s", v[0])
		case If:
			fmt.Fprintf(w, "if(")
			ex(v.If)
			fmt.Fprintf(w, "){\n")
			ex(v.Then)
			if len(v.Else) > 0 {
				fmt.Fprintf(w, "} else {\n")
				ex(v.Else)
			}
			fmt.Fprintf(w, "}\n")
		case Literal:
			fmt.Fprintf(w, "%s", v.Value)
			if v.Type == F64 && strings.IndexAny(v.Value, ".e") < 0 {
				fmt.Fprintf(w, ".0")
			}
			if v.Type == U32 || v.Type == U64 {
				fmt.Fprintf(w, "u")
			}
			if v.Type == I64 || v.Type == U64 {
				fmt.Fprintf(w, "l")
			}
		case LocalGet:
			fmt.Fprintf(w, "%s", locl[string(v)])
		case LocalGets:
			if len(v) != 1 {
				panic("1!=#LocalGets")
			}
			fmt.Fprintf(w, "%s", locl[v[0]])
		case Nop:
			fmt.Fprintf(w, "/*nop*/")
		case Return:
			fmt.Fprintf(w, "return ")
			if len(v) > 1 {
				panic("multi-return")
			}
			if len(v) == 1 {
				ex(v[0])
			}
			fmt.Fprintf(w, ";")
		case Stmts:
			for i := range v {
				ex(v[i])
				fmt.Fprintf(w, ";\n")
			}
		case Switch:
			fmt.Fprintf(w, "switch(")
			ex(v.E)
			fmt.Fprintf(w, ") {\n")
			for i := range v.Case {
				fmt.Fprintf(w, "case %d:{\n", i)
				ex(v.Case[i])
				fmt.Fprintf(w, "break;}\n")
			}
			if len(v.Def) > 0 {
				fmt.Fprintf(w, "default:{\n")
				ex(v.Def)
				fmt.Fprintf(w, "}}\n")
			}
		case Unary:
			fmt.Fprintf(w, "%s", opa(v.Op.Name))
			em(v.X)
		default:
			panic(fmt.Sprintf("nyi: %T", v))
		}
	}
	for _, g := range m.Globals {
		for i := range g.Name {
			if g.Const[i] {
				fmt.Fprintf(w, "const ")
			}
			fmt.Fprintf(w, "%s %s=", typ(g.Typs[i]), g.Name[i])
			ex(g.Expr[0])
			fmt.Fprintf(w, ";\n")
		}
	}

	trimname := func(s string) string {
		if c := s[len(s)-1]; len(s) > 1 && c >= '0' && c <= '9' {
			s = s[:len(s)-1]
		}
		if c := s[len(s)-1]; len(s) > 1 && c >= '0' && c <= '9' {
			s = s[:len(s)-1]
		}
		return s
	}
	form := map[Type]string{
		I32: "%d", U32: "%u", I64: "%ld", U64: "%lu", F64: "%lf",
	}
	body := func(f Func) {
		allg := make(map[string]bool)
		for _, g := range m.Globals {
			for i := range g.Name {
				allg[g.Name[i]] = true
			}
		}

		locl = make(map[string]string)
		for _, v := range f.Args {
			locl[v.Name] = v.Name
		}
		for _, v := range f.Locs {
			locl[v.Name] = v.Name
		}
		for _, v := range f.Locs {
			if s := trimname(v.Name); s != v.Name {
				if _, ok := locl[s]; !ok {
					locl[v.Name] = s
					locl[s] = s
				}
			}
		}

		decl = make(map[string]bool)
		for g := range allg {
			decl[g] = true
		}
		for _, v := range f.Locs {
			decl[v.Name] = false
		}
		for _, v := range f.Args {
			decl[v.Name] = true
		}

		if false {
			w.Write([]byte(`printf("%s`))
			for _, x := range f.Args {
				s, o := form[x.Type]
				if !o {
					panic("type: " + string(x.Type))
				}
				w.Write([]byte("," + s))
			}
			w.Write([]byte(`\n", __func__`))
			for _, x := range f.Args {
				fmt.Fprintf(w, ", %s", x.Name)
			}
			fmt.Fprintf(w, ");\n")
		}
		ex(f.Body)
	}
	f := func(dec bool) {
		for _, f := range m.Funcs {
			rt := "void"
			if len(f.Rets) > 0 {
				rt = typ(f.Rets[0])
			}
			var a []string
			for _, x := range f.Args {
				s := typ(x.Type)
				if !dec {
					s += " " + x.Name
				}
				a = append(a, s)
			}
			fmt.Fprintf(w, "%s %s(%s)", rt, fname(f.Name), strings.Join(a, ", "))
			if dec {
				fmt.Fprintf(w, ";\n")
			} else {
				fmt.Fprintf(w, "{\n")
				if f.Defer != nil {
					fmt.Fprintf(w, "if(!setjmp(jb_)){\n")
				}
				body(f)
				if f.Defer != nil {
					fmt.Fprintf(w, "}else{\n")
					ex(f.Defer)
					fmt.Fprintf(w, ";\n}\n")
				}
				fmt.Fprintf(w, "}\n")
			}
		}
	}
	f(true)
	f(false)

	ch := strings.Replace(chead, "#FN", strconv.Itoa(maxfn), 1)
	ww.Write([]byte(ch)[1:])

	w.Flush()

	fmt.Fprintf(ww, cmain1[1:])
	for _, t := range m.Table {
		for j, s := range t.Names {
			i := t.Off + j
			fmt.Fprintf(ww, "$fn[%d]=(void*)%s;\n", i, s)
		}
	}
	for _, d := range m.Data {
		s := make([]byte, 2*len(d.Data))
		hex.Encode(s, []byte(d.Data))
		b := make([]byte, 2*len(s))
		for i := 0; i < len(s); i += 2 {
			b[2*i+0] = '\\'
			b[2*i+1] = 'x'
			b[2*i+2] = s[i]
			b[2*i+3] = s[1+i]
		}
		fmt.Fprintf(ww, "memcpy($c+%d,\"%s\",%d);\n", d.Off, string(b), len(d.Data))
	}
	fmt.Fprintf(ww, cmain2[1:])

}

type cfmt struct {
	b *bytes.Buffer
	w io.Writer
}

func (c cfmt) Write(b []byte) (int, error) { return c.b.Write(b) }
func (c cfmt) Flush() {
	b := c.b.Bytes()
	v := bytes.Split(b, []byte("\n"))
	var s []byte
	for _, w := range v {
		if len(w) == 0 || len(w) == 1 && w[0] == ';' {
			continue
		}
		if bytes.HasSuffix(w, []byte("};")) {
			w = w[:len(w)-1]
		}
		if bytes.HasSuffix(w, []byte(";;")) {
			w = w[:len(w)-1]
		}
		ic := bytes.HasPrefix(w, []byte("case ")) || bytes.HasPrefix(w, []byte("default:"))
		if w[0] == '}' || ic {
			s = s[:len(s)-1]
		}
		c.w.Write(s)
		c.w.Write(w)
		c.w.Write([]byte{10})
		if w[len(w)-1] == '{' || ic {
			s = append(s, 32)
		}
	}
}

const chead = `
#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>
#include<string.h>
#include<setjmp.h>
#include<math.h>
#define F64abs       fabs
#define F64sqrt      sqrt
#define F64floor     floor
#define F64min       fmin
#define F64max       fmax
#define F64copysign  copysign
#define Exit exit
#define V5 __attribute((vector_size(32),aligned(32)))
typedef  int32_t i32;
typedef uint32_t u32;
typedef  int64_t i64;
typedef uint64_t u64;
typedef double   f64;
typedef int8_t   VC V5;
typedef int32_t  VI V5;
typedef double   VF V5;
static void*$fn[#FN];
static VC $C[1<<24];VI*$I;VF*$F;
static VC $C2[1<<24];
static char *$c,*$c2;
static i32 *$i;
static i64 *$j;
static f64 *$f;
static int32_t memorysize_, memorysize2_;
static int    args_;
static char **argv_;
static jmp_buf jb_;
#define I8(x)  (i32)$c[x]
#define I32(x) $i[(x)>>2]
#define I64(x) $j[(x)>>3]
#define F64(x) $f[(x)>>3]
#define SetI8(x,y)  $c[x]=((int8_t)(y))
#define SetI32(x,y) $i[(x)>>2]=(y)
#define SetI64(x,y) $j[(x)>>3]=(y)
#define SetF64(x,y) $f[(x)>>3]=(y)
#define VIload(x)    $I[(x)>>5]
#define VFload(x)    $F[(x)>>5]
#define VIstore(x,y) $I[(x)>>5]=(y)
#define VFstore(x,y) $F[(x)>>5]=(y)
#define VIloadB(x) __builtin_convertvector(*(int8_t __attribute((vector_size(8)))*)&$i[x>>2],VI)
#define I32B(x)     (int32_t)(x)
#ifdef __AVX2__
static VF VFsqrt(VF x){
 VF r={sqrt(x[0]), sqrt(x[1]), sqrt(x[2]), sqrt(x[3])};
 return r;
}
#endif
static int32_t Memorysize(void){return memorysize_; }
static int32_t Memorysize2(void){return memorysize2_;}
static int32_t Memorygrow(int32_t delta){
 int32_t r=memorysize_;
 memorysize_+=delta;
 return r;
}
static int32_t Memorygrow2(int32_t delta){
 int32_t r=memorysize2_;
 memorysize2_+=delta;
 return r;
}
static void Memorycopy (int32_t dst, int32_t src, int32_t n){ memcpy($c +dst, $c +src, (size_t)n); }
static void Memorycopy2(int32_t dst, int32_t src, int32_t n){ memcpy($c2+dst, $c2+src, (size_t)n); }
static void Memorycopy3(int32_t dst, int32_t src, int32_t n){ memcpy($c +dst, $c2+src, (size_t)n); }
//static void Memoryfill(int32_t p, int32_t v, int32_t n){ memset($c+p, (int)v, (size_t)n); }
static int32_t  I32clz(int32_t x) { return (int32_t)__builtin_clz((unsigned int)x); }
static double   F64reinterpret_i64(uint64_t x){union{uint64_t i;double f;}u;u.i=x;return u.f;}
static uint64_t I64reinterpret_f64(double   x){union{uint64_t i;double f;}u;u.f=x;return u.i;}
static int32_t Args(void){ return args_; }
static int32_t Arg(int32_t i, int32_t r){
 if(i>=args_) return 0;
 if(r ==   0) return (int32_t)strlen(argv_[i]);
 memcpy($c+r,argv_[i],strlen(argv_[i]));
 return 0;
}
static int32_t Read(int32_t file, int32_t nfile, int32_t dst){
 static char *filebuf = NULL;
 static size_t      n = 0;
 if(dst != 0){ memcpy($c+dst,filebuf,n); return 0; }
 char name[512];
 if(nfile > 511) return -1;
 memcpy(name, $c+file, (size_t)nfile);
 name[nfile] = (char)0;
 FILE *fp = fopen(name, "rb");
 if(fp==NULL){if(filebuf!=NULL)free(filebuf);n=0;return -1;}
 fseek(fp, 0, SEEK_END);
 n=(size_t)ftell(fp);
 fseek(fp, 0, SEEK_SET);
 if(filebuf != NULL) free(filebuf);
 filebuf = malloc(n);
 if(n != fread(filebuf, 1, n, fp)){ fclose(fp); return -1; }
 fclose(fp);
 return (int32_t)n;
}
static int32_t Write(int32_t file, int32_t nfile, int32_t src, int32_t n){
 if(nfile == 0){ fwrite($c+src, 1, (size_t)n, stdout); return 0; }
 char name[512];
 memcpy(name, $c+file, (size_t)nfile);
 name[nfile] = (char)0;
 FILE *fp = fopen(name, "wb");
 if(fp == NULL){ return -1; }
 fwrite($c+src, 1, (size_t)n, fp);
 fclose(fp);
 return 0;
}
static int32_t ReadIn(int32_t dst, int32_t n){
 char *r = fgets($c+dst, n, stdin);
 if(r==NULL){ //todo eof
  return 0;
 }else return (int32_t)strnlen($c+dst,(size_t)n);
}
static int64_t Native(int64_t x, int64_t y){
#ifdef NATIVE
 return cnative(x, y);
#else
 return 0*(x+y);
#endif
}
static void panic(int32_t x) { longjmp(jb_,1); }
`
const cmain1 = `
int main(int args, char **argv){
 args_=(int32_t)args;
 argv_=argv;
 $c=(char*)$C;
 $c2=(char*)$C2;
 $i=(i32*)$C;
 $j=(i64*)$C;
 $f=(f64*)$C;
 $I=(VI*)$C;
 $F=(VF*)$C;
`
const cmain2 = `
 main_();
 return 0;
}
`
