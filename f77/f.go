package f77

import (
	"bytes"
	"fmt"
	"io"
	"os"
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
var cmp_ map[string]string
var fun map[string]wg.Func
var typ map[string]wg.Type // name77
var GLO map[string]wg.Type // name77 (all globals)
var glo map[string]bool    // name77 (within a function)
var CUR wg.Func            // current function
var heap bool              // func uses heap
var lab map[string]int     // label counter
var stk []string
var glb []byte // global declarations
var w io.Writer

func init() {
	reserved = make(map[string]bool)
	for _, s := range strings.Split("MAIN COMMON WRITE FUNCTION SUBROUTINE INT REAL IB", " ") {
		reserved[s] = true
	}
	t77_ = map[wg.Type]string{
		wg.U32: "INTEGER*4",
		wg.I32: "INTEGER*4",
		wg.I64: "INTEGER*8",
		wg.U64: "INTEGER*8",
		wg.F64: "REAL*8",
	}
	bop_ = map[wg.Op]string{}
	for _, c := range []string{"+.", "-.", "*.", "/."} {
		s := strings.TrimSuffix(c, ".")
		for _, t := range []wg.Type{wg.I32, wg.U32, wg.I64, wg.I64} {
			bop_[wg.Op{s, t}] = s
		}
		if strings.HasSuffix(c, ".") {
			bop_[wg.Op{s, wg.F64}] = s
		}
	}
	cmp_ = map[string]string{}
	o := strings.Split("LT.LE.GT.GE.EQ.NE.AND", ".")
	for i, v := range strings.Split("< <= > >= == != &&", " ") {
		cmp_[v] = o[i]
	}
	mop_ = map[wg.Op]string{}
	for _, t := range []wg.Type{wg.I32, wg.U32, wg.I64, wg.I64} {
		mop_[wg.Op{"-", t}] = "-"
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
		fn(m, i)
	}
	w.Write([]byte(builtins))
	out.Write(indent77(buf.Bytes()))
}
func t77(t wg.Type) string {
	r, o := t77_[t]
	if !o {
		panic(fmt.Sprintf("type %v nyi", t))
	}
	return r
}

func fn(m wg.Module, k int) {
	f := m.Funcs[k]
	CUR = f

	fmt.Fprintf(os.Stderr, "func %s\n", f.Name)

	_ll, ll_, heap_, lab_ := _loc, loc_, heap, lab
	initLocs(f)
	heap = false
	lab = make(map[string]int)
	defer func() {
		_loc, loc_, heap, lab = _ll, ll_, heap_, lab_
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
			fmt.Fprintln(os.Stderr, "loc", _loc, loc_)
			fmt.Fprintln(os.Stderr, "typ", typ)
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
	w.Write(del(buf.Bytes()))
	fmt.Fprintf(w, "RETURN\nEND\n")
}
func simple(f wg.Func) bool { return len(f.Rets) == 1 && len(f.Args) == 1 }

func emit(x wg.Emitter) {
	switch v := x.(type) {
	case wg.Stmts:
		stmts(v)
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
	case wg.If:
		iff(v)
	case wg.For:
		do(v)
	case wg.Branch:
		branch(v)
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

func stmts(s wg.Stmts) {
	for _, e := range s {
		emit(e)
	}
}
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

	if cop, o := cmp_[b.Op.Name]; o {
		fmt.Fprintf(w, "%s = IB(%s .%s. %s)\n", s, x, cop, y)
		return
	}

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
	holer := func(bits int, s string) string {
		var u uint64
		var e error
		if strings.HasPrefix(s, "-") {
			var i int64
			i, e = strconv.ParseInt(s, 10, bits)
			u = uint64(i)
		} else {
			u, e = strconv.ParseUint(s, 10, bits)
		}
		if e != nil {
			panic("cannot parse int literal: " + s)
		}
		return fmt.Sprintf("z'%X'", u)
	}
	bits := atoi(l.Type.String()[1:])
	switch l.Type {
	case wg.I32, wg.U32, wg.U64, wg.I64:
		if l.Type == wg.I32 {
			i := atoi(l.Value)
			if i < 100 && i > -100 {
				return fmt.Sprintf("%d", i)
			}
		}
		return fmt.Sprintf("INT(%s,%d)", holer(bits, l.Value), bits/8)
	case wg.F64:
		return fmt.Sprintf("REAL(%s,8)", double(l.Value))
	default:
		panic(fmt.Sprintf("literal: type nyi: %v", l.Type))
	}
}
func assign(a wg.Assign) {
	if len(a.Name) > 1 && len(a.Expr) == 1 {
		multiassign(a)
		return
	}
	for i, s := range a.Name {
		if len(a.Const) > 0 && a.Const[i] {
			panic("assign const should not happen") // only in initialization
		}
		mod := ""
		switch a.Mod {
		case ":=", "=", "":
		case "+=", "*=":
			mod = strings.TrimSuffix(a.Mod, "=")
		default:
			panic("modified assign nyi: " + a.Mod)
		}
		x := ev(a.Expr[i])

		if a.Glob[i] {
			s = sym(s)
			glo[s] = true
		} else {
			s = loc(s)
		}
		if mod == "" {
			fmt.Fprintf(w, "%s = %s\n", s, x)
		} else {
			fmt.Fprintf(w, "%s = %s %s %s\n", s, s, mod, x)
		}
	}
}
func multiassign(a wg.Assign) {
	ex := a.Expr[0]
	_, o := ex.(wg.Call)
	if o == false {
		panic(fmt.Sprintf("multiassign expected call: %T\n", ex))
	}
	emit(ex)
	for i := len(a.Name) - 1; i >= 0; i-- {
		s := a.Name[i]
		if a.Glob[i] {
			s = sym(s)
			glo[s] = true
		} else {
			s = loc(s)
		}
		fmt.Fprintf(w, "%s = %v\n", s, pop())
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
func iff(i wg.If) {

	/*
		b, o := i.If.(wg.Binary)
		if o == false {
			panic("if: expected binary")
		}
		op, o := cmp_[b.Op.Name]
		if o == false {
			panic("if: expected comparison")
		}
		x, y := ev(b.X), ev(b.Y)
	*/
	x, y, op := binop(i.If)

	fmt.Fprintf(w, "IF(%s .%s. %s)THEN\n", x, op, y)
	emit(i.Then)
	if i.Else != nil {
		fmt.Fprintf(w, "ELSE\n")
		emit(i.Else)
	}
	fmt.Fprintf(w, "ENDIF\n")
}
func binop(e wg.Expr) (string, string, string) {
	b, o := e.(wg.Binary)
	if o == false {
		panic("expected binary")
	}
	op, o := cmp_[b.Op.Name]
	if o == false {
		panic("expected comparison")
	}
	x, y := ev(b.X), ev(b.Y)
	return x, y, op
}
func do(f wg.For) {
	// 10 continue
	//    if( condition ) then
	//        body
	// 11 continue
	//        post
	//        goto 10
	//    end if
	// 15 continue

	// there is no short (f.Simple) variant

	l := 10 * (1 + len(lab))
	s := f.Label
	if s == "" {
		s = strconv.Itoa(l)
	}
	lab[s] = l

	fmt.Fprintf(w, ":%d:CONTINUE\n", l) // : will be stripped by indent()
	if f.Cond != nil {
		x, y, op := binop(f.Cond)
		fmt.Fprintf(w, "IF(%s .%s. %s)THEN\n", x, op, y)
	}
	if f.Body != nil {
		emit(f.Body)
	}
	fmt.Fprintf(w, ":%d:CONTINUE\n", l+1) // target for continue
	if f.Post != nil {
		emit(f.Post)
	}
	if f.Cond != nil {
		fmt.Fprintf(w, "GOTO %d\nENDIF\n", l)
	}
	fmt.Fprintf(w, ":%d:CONTINUE\n", l+5) // target for break
}
func branch(b wg.Branch) { // break/continue
	l := 10 * len(lab)
	if b.Label != "" {
		l = lab[b.Label]
	}
	if b.Break { // break
		fmt.Fprintf(w, "GOTO %d\n", l+5)
	} else { // continue
		fmt.Fprintf(w, "GOTO %d\n", l+1)
	}
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
func del(b []byte) (r []byte) { // delete unused labels
	v := bytes.Split(b, []byte{10})
	m := make(map[int]bool)
	for i := range v {
		if bytes.HasPrefix(v[i], []byte("GOTO ")) {
			s := v[i]
			m[atoi(string(s[5:]))] = true
		}
	}
	for _, b := range v {
		s := string(b)
		if strings.HasPrefix(s, ":") {
			s = s[1:]
			p := strings.Index(s, ":")
			l := atoi(s[:p])
			if m[l] == false {
				continue
			}
		}
		if r == nil {
			r = append(r, b...)
		} else {
			r = append(r, byte('\n'))
			r = append(r, b...)
		}
	}
	return r
}
func indent77(b []byte) (r []byte) {
	v := bytes.Split(b, []byte{10})
	l := 6
	for i := range v {
		s := string(v[i])
		if anyprefix(s, []string{"THEN", "ENDIF"}) {
			l--
		}
		if len(s) > 0 && s[0] == ':' { // :label:...
			s = s[1:]
			p := strings.Index(s, ":")
			l := atoi(s[:p])
			v[i] = []byte(fmt.Sprintf("%-6d%s", l, s[1+p:]))
		} else {
			r = append(r, bytes.Repeat([]byte(" "), l)...)
		}
		r = append(r, v[i]...)
		r = append(r, 10)

		if anyprefix(s, []string{"IF("}) {
			l++
		}
	}
	return r
}
func anyprefix(s string, v []string) bool {
	for i := range v {
		if strings.HasPrefix(s, v[i]) {
			return true
		}
	}
	return false
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
		s := addsym(l.Name, _loc, loc_)
		typ[s] = l.Type
	}
	for _, l := range f.Locs {
		s := addsym(l.Name, _loc, loc_)
		typ[s] = l.Type
	}
	if simple(f) == false {
		for i := range f.Rets {
			s := addsym(rname(i), _loc, loc_)
			typ[s] = f.Rets[i]
		}
	}
}
func addsym(s string, _m map[string]string, m_ map[string]string) string {
	v := strings.ToUpper(s)
	v = strings.ReplaceAll(v, ".", "")
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
const builtins = `LOGICAL FUNCTION IB(X)
INTEGER*4 X
IB = X .NE. 0
RETURN
END
`
