package f77

import (
	"bytes"
	"fmt"
	"io"
	"sort"
	"strconv"
	"strings"

	"github.com/ktye/wg"
)

// todo
// - remove implicit none
// - filter double return

var reserved map[string]bool
var t77_ map[wg.Type]string
var sym_ map[string]string
var _sym map[string]string
var _loc map[string]string
var loc_ map[string]string
var bop_ map[wg.Op]string
var mop_ map[wg.Op]string
var fun map[string]wg.Func
var typ map[string]wg.Type // name77
var GLO map[string]wg.Type // name77 (all globals)
var glo map[string]bool    // name77 (within a function)
var CUR wg.Func            // current function
var heap bool              // func uses heap
var stk []string
var glb []byte // global declarations
var w io.Writer

func init() {
	reserved = make(map[string]bool)
	for _, s := range strings.Split("MAIN COMMON WRITE FUNCTION SUBROUTINE INT REAL", " ") {
		reserved[s] = true
	}
	t77_ = map[wg.Type]string{
		wg.U32: "INTEGER*4",
		wg.I32: "INTEGER*4",
		wg.I64: "INTEGER*8",
		wg.U64: "INTEGER*8",
		wg.F64: "REAL*8",
	}
	bop_ = map[wg.Op]string{
		{"+", wg.I32}: "+",
	}
	mop_ = map[wg.Op]string{
		{"-", wg.I32}: "-",
		{"-", wg.F64}: "-",
	}
}

func F(out io.Writer, m wg.Module) {
	initSyms(m)
	stk = make([]string, 0)
	fun = make(map[string]wg.Func)
	typ = make(map[string]wg.Type)
	GLO = make(map[string]wg.Type)

	g := make([]string, 0)
	l := make([]string, 0)
	{ // globals
		var buf bytes.Buffer
		w = &buf
		d := make(map[string][]string)
		for _, a := range m.Globals {
			for i, s := range a.Name {
				e := a.Expr[i]
				lit, o := e.(wg.Literal)
				if o == false {
					panic(fmt.Sprintf("global declaration is a %T", e))
				}
				s = addsym(s, _sym, sym_)
				g = append(g, s)
				l = append(l, litstr(lit))
				t := t77(a.Typs[i])
				d[t] = append(d[t], s)
				GLO[s] = a.Typs[i]
			}
		}
		declare(d)
		commons(g)
		glb = buf.Bytes()
	}

	var buf bytes.Buffer
	w = &buf

	w.Write([]byte(fhead))
	w.Write([]byte(mem))
	w.Write(glb)

	for i := range g { // global initialization
		fmt.Fprintf(w, "%s = %s\n", g[i], l[i])
	}

	fmt.Fprintf(w, "CALL %s\n", sym("main"))
	fmt.Fprintf(w, "STOP\nEND\n")
	for _, f := range m.Funcs {
		fun[f.Name] = f
	}
	for i := range m.Funcs {
		f77Func(m, i)
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

func f77Func(m wg.Module, k int) {
	f := m.Funcs[k]
	CUR = f

	_ll, ll_, heap_ := _loc, loc_, heap
	initLocs(f)
	heap = false
	defer func() {
		_loc, loc_, heap = _ll, ll_, heap_
	}()

	v := make([]string, len(f.Args))
	for i := 0; i < len(f.Args); i++ {
		v[i] = loc(f.Args[i].Name)
		typ[v[i]] = f.Args[i].Type
	}
	if simple(f) == false {
		for i := range f.Rets {
			r := loc(rname(i))
			v = append(v, r)
			typ[r] = f.Rets[i]
		}
	}
	for _, l := range f.Locs {
		s := loc(l.Name)
		typ[s] = l.Type
	}
	glo = make(map[string]bool)

	if simple(f) {
		fmt.Fprintf(w, "%s FUNCTION %s(%s)\n", t77(f.Rets[0]), sym(f.Name), v[0])
	} else {
		fmt.Fprintf(w, "SUBROUTINE %s(%s)\n", sym(f.Name), strings.Join(v, ","))
	}
	fmt.Fprintf(w, "IMPLICIT NONE\n")

	w_ := w
	var buf bytes.Buffer
	w = &buf
	for _, st := range f.Body {
		emit(st)
	}
	w = w_

	if heap { // only write heap declaration if the function uses it
		w.Write([]byte(mem))
	}
	d := make(map[string][]string)
	for v := range _loc {
		tt, o := typ[v]
		if o == false {
			fmt.Println("loc", _loc)
			fmt.Println("typ", typ)
			panic(fmt.Sprintf("unknown type for %v", v))
		}
		t := t77(tt)
		d[t] = append(d[t], v)
	}
	declare(d)
	{ // declare globals used within the subroutine
		var v []string
		for k := range glo {
			v = append(v, k)
		}
		sort.Strings(v)
		for _, s := range v {
			fmt.Fprintf(w, "%s %s\n", t77(GLO[s]), s)
		}
		for _, s := range v {
			fmt.Fprintf(w, "COMMON /%s/ %s\n", s, s)
		}
	}

	io.Copy(w, &buf)
	fmt.Fprintf(w, "RETURN\nEND\n")
}
func simple(f wg.Func) bool { return len(f.Rets) == 1 && len(f.Args) == 1 }

func emit(x wg.Emitter) {
	switch v := x.(type) {
	case wg.Return:
		retrn(v)
	case wg.Cast:
		cast(v)
	case wg.Binary:
		binary(v)
	case wg.Unary:
		unary(v)
	case wg.LocalGet:
		localget(v)
	case wg.LocalGets:
		localgets(v)
	case wg.GlobalGet:
		globalget(v)
	case wg.GlobalGets:
		globalgets(v)
	case wg.Drop:
		emit(v.Expr)
	case wg.Call:
		call(v)
	case wg.Literal:
		literal(v)
	case wg.Assign:
		assign(v)
	case wg.Printf:
		printf(v)
	default:
		panic(fmt.Sprintf("f77-emit not implemented: %T", x))
	}
}

/*
func (b Branch) f(w io.Writer)       { panic("nyi") }
func (s Stmts) f(w io.Writer)        { panic("nyi") }
func (s Switch) f(w io.Writer)       { panic("nyi") }
func (n Nop) f(w io.Writer)          {}
func (i If) f(w io.Writer)           { panic("nyi") }
func (f For) f(w io.Writer)          { panic("nyi") }
func (c CallIndirect) f(w io.Writer) { panic("nyi") }
*/

func retrn(r wg.Return) {
	if simple(CUR) {
		if len(r) != 1 {
			panic("expected 1 return value")
		}
		emit(r[0])
		fmt.Fprintf(w, "%s = %s\nRETURN\n", sym(CUR.Name), pop())
		return
	}
	for i, e := range r {
		emit(e)
		fmt.Fprintf(w, "%s = %s\n", loc(rname(i)), pop())
	}
	fmt.Fprintf(w, "RETURN\n")
}
func cast(c wg.Cast) {
	if c.Dst == c.Src {
		push(ev(c.Arg))
		return
	}
	switch c.Dst {
	case wg.I32, wg.U32:
		push("INT(" + ev(c.Arg) + ",4)")
	case wg.I64, wg.U64:
		push("INT(" + ev(c.Arg) + ",8)")
	case wg.F64:
		push("REAL(" + ev(c.Arg) + ",8)")
	default:
		// unsigned (wg.U64)  may print like signed "i64"
		panic(fmt.Sprintf("nyi: cast to %v from %v", c.Dst, c.Src))
	}
}
func binary(b wg.Binary) {
	x, y, s := ev(b.X), ev(b.Y), ssa(b.Op.Type)

	f := ""
	switch b.Op.Name {
	case "<<", ">>":
		f = "SHIFTR"
		if b.Op.Type == wg.U32 || b.Op.Type == wg.U64 {
			f = "SHIFTA"
		}
		if b.Op.Name == "<<" {
			f = "SHIFTL"
		}
	case "|":
		f = "IOR"
	}
	if f == "" {
		fmt.Fprintf(w, "%s = %s %s %s\n", s, x, bop(b.Op), y)
	} else {
		fmt.Fprintf(w, "%s = %s(%s, %s)\n", s, f, x, y)
	}
}
func unary(u wg.Unary) {
	x := ev(u.X)
	push("(" + mop(u.Op) + x + ")")
}
func localget(l wg.LocalGet) { stk = append(stk, loc(string(l))) }
func localgets(l wg.LocalGets) {
	for _, s := range l {
		emit(wg.LocalGet(s))
	}
}
func globalget(g wg.GlobalGet) {
	s := sym(string(g))
	glo[s] = true
	stk = append(stk, s)
}
func globalgets(g wg.GlobalGets) {
	for _, gg := range g {
		globalget(wg.GlobalGet(gg))
	}
}
func bop(op wg.Op) string {
	s, o := bop_[op]
	if !o {
		panic(fmt.Sprintf("binary operator nyi: %+v", op))
	}
	return s
}
func mop(op wg.Op) string {
	s, o := mop_[op]
	if !o {
		panic(fmt.Sprintf("unary operator nyi: %+v\n", op))
	}
	return s
}
func literal(l wg.Literal) { push(litstr(l)) }
func litstr(l wg.Literal) string {
	double := func(s string) string {
		f, e := strconv.ParseFloat(s, 64)
		if e != nil {
			panic("cannot parse float literal: " + s)
		}
		return strings.Replace(strconv.FormatFloat(f, 'e', -1, 64), "e", "D", 1)
	}
	switch l.Type {
	case wg.I32: // ssa(wg.I32)
		return fmt.Sprintf("INT(%s,4)", l.Value)
	case wg.U64:
		u, e := strconv.ParseUint(l.Value, 10, 64)
		if e != nil {
			panic("cannot parse u64 literal: " + l.Value)
		}
		i := int64(u)
		return fmt.Sprintf("INT(%d,8)", i)
	case wg.I64:
		return fmt.Sprintf("INT(%s,8)", l.Value)
	case wg.F64:
		return fmt.Sprintf("REAL(%s,8)", double(l.Value))
	default:
		panic(fmt.Sprintf("literal: type nyi: %v", l.Type))
	}
}
func assign(a wg.Assign) {
	for i, s := range a.Name {
		if len(a.Const) > 0 && a.Const[i] {
			panic("assign const should not happen") // only in initialization
		}
		if a.Mod == ":=" {
		} else if a.Mod == "=" {
		} else if a.Mod != "" {
			panic("modified assign nyi: " + a.Mod)
		}
		x := ev(a.Expr[i])

		if a.Glob[i] {
			s = sym(s)
			glo[s] = true
		} else {
			s = loc(s)
		}
		fmt.Fprintf(w, "%s = %s\n", s, x)
	}
}
func call(c wg.Call) {
	args := make([]string, 0)
	for _, a := range c.Args {
		args = append(args, ev(a))
	}

	if builtinCall(c, args) {
		return
	}

	f := fun[c.Func]
	if simple(f) == false {
		for _, t := range f.Rets {
			args = append(args, ssa(t))
		}
		fmt.Fprintf(w, "CALL %s(%s)\n", sym(c.Func), strings.Join(args, ","))
	} else {
		// the called function must be declared like a local
		name := sym(c.Func)
		_loc[name] = "*function call*"
		typ[name] = f.Rets[0]
		push(name + "(" + args[0] + ")")
	}
}
func builtinCall(c wg.Call, a []string) bool {
	s := c.Func
	switch s {
	case "I8", "SetI8", "I32", "SetI32", "I64", "U64", "SetU64", "SetI64", "F64", "SetF64":
		heap = true
		set := false
		if strings.HasPrefix(s, "Set") {
			s = strings.TrimPrefix(s, "Set")
			set = true
		}
		c := 'I'
		if strings.HasPrefix(s, "F") {
			c = 'F'
		}
		sz := 8
		if strings.HasSuffix(s, "8") == false {
			sz = atoi(s[len(s)-2:])
		}
		div := sz / 8
		addr := fmt.Sprintf("1+(%s/%d)", a[0], div)
		{

			s := strings.TrimSuffix(strings.TrimPrefix(a[0], "INT("), ",4)")
			a, e := strconv.ParseInt(s, 10, 64)
			a /= int64(div)
			if e == nil {
				addr = fmt.Sprintf("%d", 1+a) // instead of "INT(xx,4)/4"
			}

		}
		if set {
			fmt.Fprintf(w, "%c%d(%s) = %s\n", c, sz, addr, a[1])
		} else {
			s := fmt.Sprintf("%c%d(%s)", c, sz, addr)
			push(s)
		}
		return true
	default:
		return false
	}
}
func printf(p wg.Printf) {
	f := strings.TrimPrefix(strings.TrimSuffix(p.Format, `"`), `"`)
	wf := "write(*,*) '%s'"
	for i := 0; i < len(p.Args); i++ {
		wf += ", " + loc(p.Args[i])
	}
	wf += "\n"
	fmt.Fprintf(w, wf, f)
}

func pop() string {
	if len(stk) == 0 {
		panic("underflow")
	}
	r := stk[len(stk)-1]
	stk = stk[:len(stk)-1]
	return r
}
func push(s string)          { stk = append(stk, s) }
func ev(e wg.Emitter) string { emit(e); return pop() }
func indent77(b []byte) (r []byte) {
	v := bytes.Split(b, []byte{10})
	for i := range v {
		r = append(r, []byte("      ")...)
		r = append(r, v[i]...)
		r = append(r, 10)
	}
	return r
}
func ssa(t wg.Type) string { // create new ssa variable
	s := addsym("q", _loc, loc_)
	push(s)
	typ[s] = t
	return s
}
func loc(s string) string {
	r, o := loc_[s]
	if o == false {
		panic("unknown local: " + s)
	}
	return r
}
func sym(s string) string { // fortran names for globals and subroutines
	r, o := sym_[s]
	if o == false {
		panic("unknown symbol: " + s)
	}
	return r
}
func initSyms(m wg.Module) {
	// first add all symbols upper(6&#s)#s globals and function names
	// sym(s) lookup create new names for those that are not unique.
	_sym, sym_ = make(map[string]string), make(map[string]string)
	for _, f := range m.Funcs {
		addsym(f.Name, _sym, sym_)
	}
	/*
		for _, a := range m.Globals {
			for _, s := range a.Name {
				addsym(s, _sym, sym_)
			}
		}
	*/
}
func initLocs(f wg.Func) {
	_loc, loc_ = make(map[string]string), make(map[string]string)
	for _, l := range f.Args {
		addsym(l.Name, _loc, loc_)
	}
	for _, l := range f.Locs {
		addsym(l.Name, _loc, loc_)
	}
	if simple(f) == false {
		for i := range f.Rets {
			addsym(rname(i), _loc, loc_)
		}
	}
}
func addsym(s string, _m map[string]string, m_ map[string]string) string {
	v := strings.ToUpper(s)
	if len(v) > 6 {
		v = v[:6]
	}
	if _, o := _m[v]; o == false && reserved[v] == false {
		//fmt.Println("addsym new", s, v)
		m_[s] = v
		_m[v] = s
		return v
	}

	i := 0
	for {
		p := fmt.Sprintf("%X", i)
		c := v + p
		if len(c) > 6 {
			c = s[:6-len(p)] + p
		}
		if _, o := _m[c]; o == false && reserved[c] == false {
			//fmt.Println("addsym create", s, c)
			_m[c] = s
			m_[s] = c
			return c
		}
		i++
	}
}
func rname(i int) string {
	if i == 0 {
		return "r"
	} else {
		return "r" + strconv.Itoa(i)
	}
}
func declare(d map[string][]string) {
	var t []string
	for k := range d {
		t = append(t, k)
	}
	sort.Strings(t)
	for _, tp := range t {
		o := tp + " "
		v := d[tp]
		sort.Strings(v)
		for i, s := range v {
			c := ","
			if o[len(o)-1] == ' ' {
				c = ""
			}
			if len(o)+len(s) < 65 {
				o += c + s
			} else {
				w.Write([]byte(o + "\n"))
				o = tp + " " + s
			}
			if i == len(v)-1 {
				w.Write([]byte(o + "\n"))
			}
		}
	}
}
func commons(v []string) {
	/*
		o := "COMMON "
		for _, s := range v {
			if len(o)+len(s) < 65 {
				if o[len(o)-1] != ' ' {
					o += ","
				}
				o += s
			} else {
				fmt.Fprintf(w, "%s\n", o)
				o = "COMMON " + s
			}
		}
		if o[len(o)-1] != ' ' {
			fmt.Fprintf(w, "%s\n", o)
		}
	*/
	for _, s := range v {
		fmt.Fprintf(w, "COMMON /%s/ %s\n", s, s)
	}
}
func atoi(s string) int {
	i, e := strconv.Atoi(s)
	if e != nil {
		panic("expected integer: " + s)
	}
	return i
}

/*
const fhead = `PROGRAM MAIN
IMPLICIT NONE
CHARACTER ABCDEFG
`
*/

const fhead = `PROGRAM MAIN
IMPLICIT NONE
`
const mem = `INTEGER*1 I8(8292)
INTEGER*4 I32(4096)
INTEGER*8 I64(1024)
REAL*8    F64(1024)
COMMON /MEM/I8
EQUIVALENCE(I8,I32,I64,F64)
`
