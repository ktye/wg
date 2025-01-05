package wg

import (
	"io"
	"fmt"
	"bytes"
	"strings"
	"strconv"
	"encoding/hex"
)

func (m Module) C(ww io.Writer) {
	w := cfmt{b:bytes.NewBuffer(nil), w:ww}

	maxfn := 0
	for _, t := range m.Table {
		if n := t.Off + len(t.Names); n > maxfn {
			maxfn = n
		}
	}
	ch := strings.Replace(chead, "#FN", strconv.Itoa(maxfn), 1)
	w.Write([]byte(ch)[1:])

	typ := func(t Type) string {
		return t.String()
	}
	fname := func(s string) string {
		if strings.Index("  main ldexp hypot atan2 atan exp log pow ", " " + s + " ") > 0 {
			return s + "_"
		}
		return s
	}
	opa := func(s string) string {
		if s == "&^" {
			return "&!"
		}
		return s
	}

	var ex func(e Expr)
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
	
	ex = func(e Expr) {
		switch v := e.(type) {
		case Assign:
			for i := range v.Name {
				mod := strings.TrimSuffix(v.Mod, "=")
				if mod == ":" {
					mod = ""
				}
				ant, tna := mod == "&^", ""
				if ant { // &^= => &= ^..
					mod, tna = "&", "~"
				}
				fmt.Fprintf(w, "%s %s= %s", v.Name[i], mod, tna)
				if len(v.Expr) < len(v.Name) {
					fmt.Fprintf(w, "0")
				} else {
					ex(v.Expr[i])
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
				fmt.Fprintf(w,"){\n")
				ex(v.Body)
				if v.Post != nil {
					ex(v.Post)
					fmt.Fprintf(w, ";\n")
				}
				fmt.Fprintf(w,"}")
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
			if v.Type == I64 || v.Type == "U64" {
				fmt.Fprintf(w, "ll");
			}
		case LocalGet:
			fmt.Fprintf(w, "%s", v)
		case LocalGets:
			if len(v) != 1 {
				panic("1!=#LocalGets")
			}
			fmt.Fprintf(w, "%s", v[0])
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
				fmt.Fprintf(w, "case %d:\n", i)
				ex(v.Case[i])
				fmt.Fprintf(w, "break;\n")
			}
			if len(v.Def) > 0 {
				fmt.Fprintf(w, "default:\n")
				ex(v.Def)
				fmt.Fprintf(w, "}\n")
			}
		case Unary:
			fmt.Fprintf(w, "%s", opa(v.Op.Name))
			ex(v.X)
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

	body := func(f Func) {
		for _, t := range []Type{ I32, U32, I64, U64, F64, VC, VI, VF } {
			var x []string
			for _, v := range f.Locs {
				if v.Type == t {
					x = append(x, v.Name)
				}
			}
			if x != nil {
				fmt.Fprintf(w, "%s %s;\n", typ(t), strings.Join(x, ", "))
			}
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
			fmt.Fprintf(w, "%s %s(%s)", rt, fname(f.Name), strings.Join(a,", "))
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
	
	fmt.Fprintf(w, cmain1[1:])
	for _, t := range m.Table {
		for j, s := range t.Names {
			i := t.Off + j
			fmt.Fprintf(w, "$fn[%d]=(void*)%s;\n", i, s)
		}
	}
	for _, d := range m.Data {
		s := make([]byte, 2*len(d.Data))
		hex.Encode(s, []byte(d.Data))
		b := make([]byte, 2*len(s))
		for i := 0; i<len(s); i+= 2 {
			b[2*i+0] = '\\'
			b[2*i+1] = 'x'
			b[2*i+2] = s[i]
			b[2*i+3] = s[1+i]
		}
		fmt.Fprintf(w, "memcpy($c+%d,\"%s\",%d);\n", d.Off, string(b), len(d.Data))
	}
	fmt.Fprintf(w, cmain2[1:])

	w.Flush()
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
#define V5 __attribute((vector_size(32),aligned(1)))
typedef  int32_t i32;
typedef uint32_t u32;
typedef  int64_t i64;
typedef uint64_t u64;
typedef double   f64;
typedef int8_t   VC V5;
typedef int32_t  VI V5;
typedef double   VF V5;
static void*$fn[#FN];
static VC $C[1<<12];VI*$I;VF*$F;
static VC $C2[1<<12];
static char *$c,*$c2;
static i32 *$i;
static i64 *$j;
static f64 *$f;
static int32_t memorysize_, memorysize2_;
static int    args_;
static char **argv_;
static jmp_buf jb_;
#define I8(x)  $c[x]
#define I32(x) $i[(x)>>2]
#define I64(x) $j[(x)>>3]
#define F64(x) $f[(x)>>3]
#define SetI8(x,y)  $c[x]=(y)
#define SetI32(x,y) $i[(x)>>2]=(y)
#define SetI64(x,y) $j[(x)>>3]=(y)
#define SetF64(x,y) $f[(x)>>3]=(y)
#define I32B(x)     (int32_t)(x)
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
static void Memoryfill(int32_t p, int32_t v, int32_t n){ memset($c+p, (int)v, (size_t)n); }
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
