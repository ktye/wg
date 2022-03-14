package f77

import (
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"

	"github.com/ktye/wg"
)

// EQUIVALANCE (reinterpret)

var t77_ map[wg.Type]string
var stk []string

func init() {
	t77_ = map[wg.Type]string{
		wg.I32: "INTEGER*4",
		wg.I64: "INTEGER*8",
		wg.F64: "REAL*8",
	}
}

func F(out io.Writer, m wg.Module) {
	var buf bytes.Buffer
	var w io.Writer = &buf
	stk = make([]string, 0)
	fmt.Fprintf(w, "PROGRAM MAIN\nIMPLICIT NONE\nstop\nend\n")
	for i := range m.Funcs {
		f77Func(w, m, i)
	}
	out.Write(indent77(buf.Bytes()))
}
func t77(t wg.Type) string {
	r, o := t77_[t]
	if !o {
		panic(fmt.Sprintf("type %v nyi", t))
	}
	return r
}

func f77Func(w io.Writer, m wg.Module, k int) {
	f := m.Funcs[k]
	v := make([]string, len(f.Args))
	t := make([]string, len(f.Args))
	for i := range v {
		v[i] = f.Args[i].Name
		t[i] = t77(f.Args[i].Type)
	}
	for i := range f.Rets {
		v = append(v, "r"+strconv.Itoa(i))
		t = append(t, t77(f.Rets[i]))
	}
	fmt.Fprintf(w, "SUBROUTINE %s(%s)\n", f.Name, strings.Join(v, ","))
	for i := range v {
		fmt.Fprintf(w, "%s %s\n", t[i], v[i])
	}
	for _, st := range f.Body {
		emit(w, st)
	}
	fmt.Fprintf(w, "RETURN\nEND\n")
}

func emit(w io.Writer, x wg.Emitter) {
	switch v := x.(type) {
	case wg.Return:
		retrn(w, v)
	case wg.Cast:
		cast(w, v)
	default:
		panic(fmt.Sprintf("f77-emit not implemented: %T", x))
	}
}

func retrn(w io.Writer, r wg.Return) {
	for i, e := range r {
		emit(w, e)
		fmt.Fprintf(w, "r%d = %s\n", i, pop())
	}
	fmt.Fprintf(w, "RETURN\n")
}
func cast(w io.Writer, c wg.Cast) { panic("nyi") }

/*
func (c Cast) f(w io.Writer)         { panic("nyi") }
func (l Literal) f(w io.Writer)      { panic("nyi") }
func (l LocalGet) f(w io.Writer)     { panic("nyi") }
func (g GlobalGet) f(w io.Writer)    { panic("nyi") }
func (b Binary) f(w io.Writer)       { panic("nyi") }
func (u Unary) f(w io.Writer)        { panic("nyi") }
func (a Assign) f(w io.Writer)       { panic("nyi") }
func (r Return) f(w io.Writer)       { panic("nyi") }
func (b Branch) f(w io.Writer)       { panic("nyi") }
func (s Stmts) f(w io.Writer)        { panic("nyi") }
func (s Switch) f(w io.Writer)       { panic("nyi") }
func (l LocalGets) f(w io.Writer)    { panic("nyi") }
func (g GlobalGets) f(w io.Writer)   { panic("nyi") }
func (n Nop) f(w io.Writer)          {}
func (i If) f(w io.Writer)           { panic("nyi") }
func (f For) f(w io.Writer)          { panic("nyi") }
func (c CallIndirect) f(w io.Writer) { panic("nyi") }
func (c Call) f(w io.Writer)         { panic("nyi") }
func (p Printf) f(w io.Writer)       { panic("nyi") }
*/

//func () f(w io.Writer) { panic("nyi") }

func pop() string {
	if len(stk) == 0 {
		panic("underflow")
	}
	r := stk[len(stk)-1]
	stk = stk[:len(stk)-1]
	return r
}
func indent77(b []byte) (r []byte) {
	v := bytes.Split(b, []byte{10})
	for i := range v {
		r = append(r, []byte("     ")...)
		r = append(r, v[i]...)
		r = append(r, 10)
	}
	return r
}

const fhead = `PROGRAM MAIN
IMPLICIT NONE
CHARACTER ABCDEFG
INTEGER*1 INT08(8291)
INTEGER*4 INT32(4096)
INTEGER*8 INT64(1024)
REAL*8    FLOAT(1024)
EQUIVALENCE(INT08,INT32,INT64,FLOAT)
`
