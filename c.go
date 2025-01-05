package wg

import (
	"io"
	"fmt"
	"bytes"
	"strings"
)

func (m Module) C(ww io.Writer) {
	w := cfmt{b:bytes.NewBuffer(nil), w:ww}
	w.Write([]byte(chead)[1:])

	typ := func(t Type) string {
		return t.String()
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
				fmt.Fprintf(w, "%s = ", v.Name[i])
				if len(v.Expr) < len(v.Name) {
					fmt.Fprintf(w, "0")
				} else {
					ex(v.Expr[i])
				}
				fmt.Fprintf(w, ";\n")
			}
		case Binary:
			em(v.X)
			fmt.Fprintf(w, "%s", v.Op.Name)
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
			fmt.Fprintf(w, "%s(", v.Func)
			for i := range v.Args {
				if i > 0 {
					fmt.Fprintf(w, ", ")
				}
				ex(v.Args[i])
			}
			fmt.Fprintf(w, ")")
		case CallIndirect:
			fmt.Fprintf(w, "F[")
			ex(v.Func)
			fmt.Fprintf(w, "](")
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
				ex(v.Body)
				fmt.Fprintf(w, "}while(")
				if v.Post != nil {
					ex(v.Post)
					fmt.Fprintf(w, ";\n")
				}
				ex(v.Cond)
				fmt.Fprintf(w, ")")
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
			if v.Type == F64 && strings.Index(v.Value, ".") < 0 {
				fmt.Fprintf(w, ".0")
			}
			if v.Type == I64 {
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
			fmt.Fprintf(w, "%s", v.Op.Name)
			ex(v.X)
		default:
			panic(fmt.Sprintf("nyi: %T", v))
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
			fmt.Fprintf(w, "%s %s(%s)", rt, f.Name, strings.Join(a,", "))
			if dec {
				fmt.Fprintf(w, ";\n")
			} else {
				fmt.Fprintf(w, "{\n")
				body(f)
				fmt.Fprintf(w, "}\n")
			}
		}
	}
	f(true)
	f(false)
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
#define V5 __attribute((vector_size(32),aligned(1)))
typedef  int32_t i32;
typedef uint32_t u32;
typedef  int64_t i64;
typedef uint64_t u64;
typedef double   f64;
typedef int8_t   VC V5;
typedef int32_t  VI V5;
typedef double   VF V5;
`
