package f77

import (
	"bytes"
	bin "encoding/binary"
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
var ret map[int]string     //name77 return var name
var fun map[string]wg.Func // fname
var typ map[string]wg.Type // name77
var con map[string]string  // constants as literals
var GLO map[string]wg.Type // name77 (all globals)
var glo map[string]bool    // name77 (within a function)
var CUR wg.Func            // current function
var shad map[string]bool   // arg is overwritten by func
var heap map[string]bool   // func uses heap "cijf"
var lab map[string]int     // label counter
var tab map[string]string  // sig->name77 (dispatch func)
var get map[string]bool    // name77 local get
var rec bool               // current func is recursive
var stk []string
var glb []byte // global declarations
var w io.Writer

const MEMSIZE = 4 * 64 * 1024

func init() {
	res := []string{
		"I8 I32 I64 F64 FNC MAIN COMMON READ WRITE FUNCTION SUBROUTINE INT REAL LEN TYPE VALUE",
		"IB IOR IAND SHIFL SHIFTR SHIFTA LEADZ ALL ANY NOT MOD MIN MAX HYPOT ATAN2 SIN COS EXP LOG FRACTION EXPONENT",
		"ABS CMPLX COMPLEX ISNAN IARGC GETARG NALLOC MEMSIZ MEMGRW XARGS XARG XWRITE XREAD XREADI",
		"BLOCK DATA BLOCKDATA EQUIVALENCE DO GOTO IF THEN ELSE END ENDIF ENDDO STOP",
	} // todo many more keywords..
	reserved = make(map[string]bool)
	for _, r := range res {
		v := strings.Split(r, " ")
		for _, s := range v {
			reserved[s] = true
		}
	}
	t77_ = map[wg.Type]string{
		wg.U32:   "INTEGER*4",
		wg.I32:   "INTEGER*4",
		wg.I64:   "INTEGER*8",
		wg.U64:   "INTEGER*8",
		wg.F64:   "REAL*8",
		wg.I8x16: "INTEGER*4",
		wg.I32x4: "INTEGER*4",
		wg.F64x2: "REAL*8",
	}
	bop_ = map[wg.Op]string{}
	for _, c := range []string{"+.", "-.", "*.", "/."} {
		s := strings.TrimSuffix(c, ".")
		for _, t := range []wg.Type{wg.I32, wg.U32, wg.I64, wg.U64} {
			bop_[wg.Op{s, t}] = s
		}
		if strings.HasSuffix(c, ".") {
			bop_[wg.Op{s, wg.F64}] = s
		}
	}
	cmp_ = map[string]string{}
	o := strings.Split("LT.LE.GT.GE.EQ.NE.AND.OR", ".")
	for i, v := range strings.Split("< <= > >= == != && ||", " ") {
		cmp_[v] = o[i]
	}
	mop_ = map[wg.Op]string{}
	for _, t := range []wg.Type{wg.I32, wg.U32, wg.I64, wg.I64, wg.F64} {
		mop_[wg.Op{"-", t}] = "-"
	}
}

func F(out io.Writer, m wg.Module) {
	stk = make([]string, 0)
	fun = make(map[string]wg.Func)
	typ = make(map[string]wg.Type)
	GLO = make(map[string]wg.Type)
	con = make(map[string]string)
	tab = make(map[string]string)
	_sym = make(map[string]string)
	sym_ = make(map[string]string)

	for _, f := range m.Funcs {
		s := addsym(f.Name, _sym, sym_)
		reserved[s] = true // don't reuse function name as local
	}

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
				if a.Const[i] == true {
					con[a.Name[i]] = litstr(lit)
					continue
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
	w.Write([]byte(memsize("INTEGER*1 I8(#1)\nCOMMON /MEM/I8\n")))
	w.Write(glb)

	for _, f := range m.Funcs {
		fun[f.Name] = f
	}
	fnc, funcs := ftab(m)
	//fmt.Fprintf(os.Stderr, "FNC: %s\n", string(fnc))
	w.Write(fnc)
	w.Write([]byte(memsize("NALLOC = SHIFTR(#1,16)\n")))

	for i := range g { // global initialization
		fmt.Fprintf(w, "%s = %s\n", g[i], l[i])
	}

	fmt.Fprintf(w, "CALL %s\n", sym("main"))
	fmt.Fprintf(w, "STOP\nEND\n")
	blockdata(m)

	w.Write(funcs) // dispatch indirect calls
	for i := range m.Funcs {
		fn(m, i)
	}
	w.Write([]byte(memsize(builtins)))
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

	//fmt.Fprintf(os.Stderr, "func %s\n", f.Name)
	if r, o := replace[f.Name]; o {
		if r == "" {
			return
		}
		r = strings.ReplaceAll(memsize(r), "?", sym(f.Name))
		r = strings.ReplaceAll(r, "@", "\n") + "\nRETURN\nEND\n"
		w.Write([]byte(r))
		return
	}
	if _, o := repmat[f.Name]; o {
		return
	}

	_ll, ll_, heap_, lab_, ret_, shad_, get_, rec_ := _loc, loc_, heap, lab, ret, shad, get, rec
	heap = make(map[string]bool)
	lab = make(map[string]int)
	ret = make(map[int]string)
	shad = make(map[string]bool)
	get = make(map[string]bool)
	rec = false
	initLocs(f)
	defer func() {
		_loc, loc_, heap, lab, ret, shad, get, rec = _ll, ll_, heap_, lab_, ret_, shad_, get_, rec_
	}()

	v := make([]string, len(f.Args))
	for i := 0; i < len(f.Args); i++ {
		v[i] = loc(f.Args[i].Name)
		//typ[v[i]] = f.Args[i].Type
	}
	//if simple(f) == false {
	for i := range f.Rets {
		r := loc(ret[i])
		v = append(v, r)
	}
	//}
	glo = make(map[string]bool)

	w_ := w
	var buf bytes.Buffer
	w = &buf
	for _, st := range f.Body {
		emit(st)
	}
	w = w_

	shd := make([][2]string, 0)
	for i, a := range f.Args {
		if shad[a.Name] { // subroutine overwrites input arg
			n := ssa(a.Type)
			shd = append(shd, [2]string{v[i], n})
			v[i] = n
		}
	}
	if rec {
		fmt.Fprintf(w, "RECURSIVE ")
	}
	if simple(f) {
		fmt.Fprintf(w, "FUNCTION %s(%s)RESULT(%s)\n", sym(f.Name), v[0], v[len(v)-1])
		//fmt.Fprintf(w, "%s FUNCTION %s(%s)\n", t77(f.Rets[0]), sym(f.Name), v[0])
	} else {
		fmt.Fprintf(w, "SUBROUTINE %s(%s)\n", sym(f.Name), strings.Join(v, ","))
	}
	fmt.Fprintf(w, "IMPLICIT NONE\n")

	if len(heap) > 0 { // only write heap declaration if the function uses it
		// INTEGER*1 I8(#1)
		// INTEGER*4 I32(#4)
		// INTEGER*8 I64(#8)
		// REAL*8    F64(#8)
		// COMMON /MEM/I8
		// EQUIVALENCE(I8,I32,I64,F64)
		fmt.Fprintf(w, "%s\n", memsize("INTEGER*1 I8(#1)"))
		eq := ""
		if heap["i"] {
			fmt.Fprintf(w, "%s\n", memsize("INTEGER*4 I32(#4)"))
			eq += ",I32"
		}
		if heap["j"] {
			fmt.Fprintf(w, "%s\n", memsize("INTEGER*8 I64(#8)"))
			eq += ",I64"
		}
		if heap["f"] {
			fmt.Fprintf(w, "%s\n", memsize("REAL*8    F64(#8)"))
			eq += ",F64"
		}
		fmt.Fprintf(w, "COMMON /MEM/I8\n")
		if eq != "" {
			fmt.Fprintf(w, "EQUIVALENCE(I8%s)\n", eq)
		}
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
	for _, x := range shd {
		fmt.Fprintf(w, "%s = %s\n", x[0], x[1])
	}
	assignLocs(f)
	w.Write(del(buf.Bytes()))
	fmt.Fprintf(w, "RETURN\nEND\n")
}
func simple(f wg.Func) bool {
	if s, o := replace[f.Name]; o && strings.HasPrefix(s, "SUBROUTINE") == false {
		return true
	}
	return len(f.Rets) == 1 && len(f.Args) == 1
}

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
		drop(v)
	case wg.Call:
		call(v)
	case wg.CallIndirect:
		calli(v)
	case wg.Literal:
		literal(v)
	case wg.Assign:
		assign(v)
	case wg.Printf:
		printf(v)
	case wg.If:
		iff(v)
	case wg.Switch:
		swtch(v)
	case wg.For:
		do(v)
	case wg.Branch:
		branch(v)
	case wg.Nop:
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
	/*
		if simple(CUR) {
			if len(r) != 1 {
				panic("expected 1 return value")
			}
			emit(r[0])
			fmt.Fprintf(w, "%s = %s\nRETURN\n", sym(CUR.Name), pop())
			return
		}
	*/
	for i, e := range r {
		emit(e)
		fmt.Fprintf(w, "%s = %s\n", loc(ret[i]), pop())
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
		s := "INT(" + ev(c.Arg) + ",8)"
		if c.Dst == wg.U64 && (c.Src == wg.I32 || c.Src == wg.U32) {
			s = "IBITS(" + s + ",0,32)" // clear sign extensions when widening
		}
		push(s)
	case wg.F64:
		push("REAL(" + ev(c.Arg) + ",8)")
	default:
		// unsigned (wg.U64)  may print like signed "i64"
		panic(fmt.Sprintf("nyi: cast to %v from %v", c.Dst, c.Src))
	}
}
func binary(b wg.Binary) {
	x, y := ev(b.X), ev(b.Y)
	if logical(b, x, y) {
		return
	}

	s := ssa(b.Op.Type)
	if cop, o := cmp_[b.Op.Name]; o {
		regret("IB", wg.I32)
		fmt.Fprintf(w, "%s = IB(%s .%s. %s)\n", s, x, cop, y)
		return
	}

	f := ""
	switch b.Op.Name {
	case "<<", ">>":
		f = "SHIFTA"
		if b.Op.Type == wg.U32 || b.Op.Type == wg.U64 {
			f = "SHIFTR"
		}
		if b.Op.Name == "<<" {
			f = "SHIFTL"
		}
	case "|":
		f = "IOR"
	case "&":
		f = "IAND"
	case "&^":
		fmt.Fprintf(w, "%s = IAND(%s,NOT(%s))\n", s, x, y)
		return
	case "%":
		f = "MOD"
	case "^":
		f = "XOR"
	}
	if f == "" {
		fmt.Fprintf(w, "%s = %s %s %s\n", s, x, bop(b.Op), y)
	} else {
		fmt.Fprintf(w, "%s = %s(%s, %s)\n", s, f, x, y)
	}
}
func logical(b wg.Binary, x, y string) bool {
	cop, o := cmp_[b.Op.Name]
	if o {
		push(fmt.Sprintf("(%s .%s. %s)", x, cop, y))
	}
	return o
}
func unary(u wg.Unary) {
	x := ev(u.X)
	push("(" + mop(u.Op) + x + ")")
}
func localget(l wg.LocalGet) {
	get[loc(string(l))] = true
	stk = append(stk, loc(string(l)))
}
func localgets(l wg.LocalGets) {
	for _, s := range l {
		emit(wg.LocalGet(s))
	}
}
func globalget(g wg.GlobalGet) {
	if s, o := con[string(g)]; o {
		push(s)
		return
	}
	s := sym(string(g))
	glo[s] = true
	stk = append(stk, s)
}
func globalgets(g wg.GlobalGets) {
	for _, gg := range g {
		globalget(wg.GlobalGet(gg))
	}
}
func drop(d wg.Drop) {
	emit(d.Expr)
	c, o := d.Expr.(wg.Call)
	f := fun[c.Func]
	if o && simple(f) { // f(x) is on stack: assign to a dummy ssa
		//fmt.Fprintf(w, "stack>: %v\n", stk)
		f := fun[c.Func]
		s := pop()
		fmt.Fprintf(w, "%s = %s\n", ssa(f.Rets[0]), s)
		pop()
		//fmt.Fprintf(w, "stack<: %v\n", stk)
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
		} else if strings.HasPrefix(s, "0x") {
			u, e = strconv.ParseUint(s[2:], 16, bits)
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
	case wg.I32, wg.U32, wg.I64, wg.U64:
		if l.Type == wg.I32 || l.Type == wg.U32 {
			i := atoi(l.Value)
			if i < 100000 && i > -100000 {
				if i < 0 {
					return fmt.Sprintf("(%d)", i)
				} else {
					return fmt.Sprintf("%d", i)
				}
			}
		} else {
			if i, e := strconv.ParseInt(l.Value, 10, 64); e == nil && i > -100000 && i < 100000 {
				return fmt.Sprintf("INT(%d,8)", i)
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
		m := map[string]string{
			"|":   "OR",
			"^":   "XOR",
			"<<":  "SHIFTL",
			">>U": "SHIFTR",
			">>":  "SHIFTA",
		}
		switch a.Mod {
		case ":=", "=", "":
		case "+=", "-=", "*=", "/=", ">>=", "^=", "|=":
			mod = strings.TrimSuffix(a.Mod, "=")
			if mod == ">>" && (a.Typs[i] == wg.I32 || a.Typs[i] == wg.I64) {
				mod = ">>U"
			}
		default:
			panic("modified assign nyi: " + a.Mod)
		}
		x := "0"
		if len(a.Expr) > 0 {
			x = ev(a.Expr[i])
		}

		if a.Glob[i] {
			s = sym(s)
			glo[s] = true
		} else {
			for _, a := range CUR.Args {
				if s == a.Name {
					shad[s] = true
				}
			}
			s = loc(s)
		}
		if mod == "" {
			fmt.Fprintf(w, "%s = %s\n", s, x)
		} else {
			r, o := m[mod]
			if o {
				fmt.Fprintf(w, "%s = %s(%s,%s)\n", s, r, s, x)
			} else {
				fmt.Fprintf(w, "%s = %s %s %s\n", s, s, mod, x)
			}
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
		if s == "_" {
			continue
		}
		if a.Glob[i] {
			s = sym(s)
			glo[s] = true
		} else {
			s = loc(s)
		}
		fmt.Fprintf(w, "%s = %v\n", s, pop())
	}
}
func calli(c wg.CallIndirect) {
	sig := sigi(c.ArgType, c.ResType)
	d, o := tab[sig]
	if o == false {
		panic("unknown dispatch func:" + sig)
	}

	e := ev(c.Func)
	simple := len(c.Args) == 1 && len(c.ResType) == 1
	if simple {
		regret(d, c.ResType[0])
		x := ev(c.Args[0])
		push(fmt.Sprintf("%s(%s,%s)", d, x, e))
	} else {
		args := make([]string, 0)
		for _, a := range c.Args {
			args = append(args, ev(a))
		}
		for _, t := range c.ResType {
			args = append(args, ssa(t))
		}
		args = append(args, e)
		fmt.Fprintf(w, "CALL %s(%s)\n", d, strings.Join(args, ","))
	}
}
func call(c wg.Call) {
	args, ismulti := multiresult(c.Args, c.Func)
	if ismulti == false {
		for _, a := range c.Args {
			args = append(args, ev(a))
		}
	}

	if builtinCall(c, args) {
		return
	}

	f := fun[c.Func]
	if CUR.Name == c.Func {
		rec = true
	}
	if simple(f) == false {
		for _, t := range f.Rets {
			args = append(args, ssa(t))
		}
		fmt.Fprintf(w, "CALL %s(%s)\n", sym(c.Func), strings.Join(args, ","))
	} else {
		name := sym(c.Func)
		if c.Func != CUR.Name {
			regret(name, f.Rets[0]) // the called function must be declared like a local
		}
		push(name + "(" + strings.Join(args, ",") + ")")
	}
}
func multiresult(e []wg.Expr, origfunc string) (args []string, ismulti bool) {
	// single argument is a function call that returns multiple results
	// e.g. func t() (int, int) {...}   func e(int, int){..}    e(t())
	args = make([]string, 0)
	if len(e) != 1 {
		return args, false
	}

	n := 0
	c, oc := e[0].(wg.CallIndirect)
	if oc {
		n = len(c.ResType)
	}
	g, og := e[0].(wg.Call)
	f := fun[g.Func]
	if og {
		n = len(f.Rets)
	}
	if (oc || og) && n > 1 {
		emit(e[0])
		args = make([]string, n)
		for i := n - 1; i >= 0; i-- {
			args[i] = pop()
		}
		return args, true
	}
	return args, false
}
func regret(fname string, t wg.Type) { // register function call as local declaration
	_loc[fname] = "*function call*"
	typ[fname] = t
}
func builtinCall(c wg.Call, a []string) bool {
	t := false
	p := func(s string) { push(s); t = true }
	s := c.Func
	switch s {
	case "I8", "U8", "SetI8", "I32", "U32", "SetI32", "I64", "U64", "SetI64", "F64", "SetF64":
		heap["c"] = true
		set := false
		if strings.HasPrefix(s, "Set") {
			s = strings.TrimPrefix(s, "Set")
			set = true
		}
		c := 'I'
		if strings.HasPrefix(s, "F") {
			c = 'F'
			heap["f"] = true
		}
		sz := 8
		if strings.HasSuffix(s, "8") == false {
			sz = atoi(s[len(s)-2:])
		}
		div := sz / 8
		addr := fmt.Sprintf("1+(%s/%d)", a[0], div)
		if div == 4 {
			heap["i"] = true
		} else if div == 8 && c != 'F' {
			heap["j"] = true
		}
		if strings.HasSuffix(addr, "/1)") {
			addr = strings.TrimSuffix(addr, "/1)") + ")"
		}
		{
			s := strings.TrimSuffix(strings.TrimPrefix(a[0], "INT("), ",4)")
			a, e := strconv.ParseInt(s, 10, 64)
			a /= int64(div)
			if e == nil {
				addr = fmt.Sprintf("%d", 1+a) // instead of "INT(xx,4)/4"
			}
		}
		if set {
			rhs := a[1]
			if div == 1 {
				rhs = "INT(" + rhs + ",1)"
			}
			fmt.Fprintf(w, "%c%d(%s) = %s\n", c, sz, addr, rhs)
		} else {
			r := fmt.Sprintf("%c%d(%s)", c, sz, addr)
			if s == "I8" || s == "U8" {
				r = "INT(" + r + ",4)"
			}
			push(r)
		}
		return true
	case "I8x16splat", "I32x4splat", "F64x2splat": //eliminate, simd functions are rewritten
		p(a[0]) // pass scalar value
	case "Memorysize":
		regret("MEMSIZ", wg.I32)
		p("MEMSIZ()")
	case "Memorygrow":
		regret("MEMGRW", wg.I32)
		p("MEMGRW(" + a[0] + ")")
	case "Memorycopy":
		fmt.Fprintf(w, "I8(1+%s:%s+%s) = I8(1+%s:%s+%s)\n", a[0], a[0], a[2], a[1], a[1], a[2])
		heap["c"] = true
		return true
	case "Memoryfill":
		fmt.Fprintf(w, "I8(1+%s:%s+%s) = INT(%s,1)\n", a[0], a[0], a[2], a[1])
		heap["c"] = true
		return true
	case "I32clz":
		p("LEADZ(" + a[0] + ")")
	case "I64popcnt":
		p("POPCNT(" + a[0] + ")")
	case "isnan":
		p("ISNAN(" + a[0] + ")")
	case "I32B":
		regret("IB", wg.I32)
		p(fmt.Sprintf("IB(%s)", a[0]))
	case "F64reinterpret_i64":
		regret("FCASTI", wg.F64)
		p("FCASTI(" + a[0] + ")")
	case "I64reinterpret_f64":
		regret("ICASTF", wg.I64)
		p("ICASTF(" + a[0] + ")")
	case "F64abs", "F64sqrt":
		p(strings.ToUpper(s[3:]) + "(" + a[0] + ")")
	case "F64floor":
		p("REAL(FLOOR(" + a[0] + ",8),8)")
	case "hypot", "atan2":
		p(strings.ToUpper(s) + "(" + a[0] + "," + a[1] + ")")
	case "F64min", "F64max":
		p(strings.ToUpper(s[3:]) + "(" + a[0] + "," + a[1] + ")")
	case "exp", "log":
		p(strings.ToUpper(s) + "(" + a[0] + ")")
	case "pow", "ipow":
		p("(" + a[0] + "**" + a[1] + ")")
	case "frexp":
		fmt.Fprintf(w, "%s = FRACTION(%s)\n", ssa(wg.F64), a[0])
		fmt.Fprintf(w, "%s = INT(INT(EXPONENT(%s),4),8)\n", ssa(wg.I64), a[0])
		return true
	case "Exit":
		fmt.Fprintf(w, "CALL EXIT("+a[0]+")\n")
		return true
	case "Args":
		p("IARGC()")
	case "Arg", "Read", "ReadIn", "Write":
		s := "X" + strings.ToUpper(s)
		if len(s) > 6 {
			s = s[:6]
		}
		regret(s, wg.I32)
		fmt.Fprintf(w, "%s = %s(%s)\n", ssa(wg.I32), s, strings.Join(a, ","))
		return true
	case "panic":
		fmt.Fprintf(w, "WRITE(*,*)'trap',%s\nCALL EXIT(1)\n", a[0])
		return true
	default:
		return false
	}
	return t
}
func printf(p wg.Printf) { // Printf("x=%d\n", x) is for debugging only
	f := strings.TrimPrefix(strings.TrimSuffix(p.Format, `"`), `"`)
	f = strings.TrimSuffix(f, "\\n")
	f = strings.ReplaceAll(f, `%12d`, "")
	wf := "write(*,*) '%s'"
	for i := 0; i < len(p.Args); i++ {
		wf += ", " + loc(p.Args[i])
	}
	wf += "\n"
	fmt.Fprintf(w, wf, f)
}
func iff(i wg.If) {
	x, y, op := binop(i.If)

	if strings.HasPrefix(x, "ISNAN(") && op == "EQ" && y == "1" {
		fmt.Fprintf(w, "IF(%s)THEN\n", x)
	} else {
		fmt.Fprintf(w, "IF(%s .%s. %s)THEN\n", x, op, y)
	}
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
		fmt.Fprintf(os.Stderr, "binop: e=%+v\n", e)
		panic("expected binary")
	}
	op, o := cmp_[b.Op.Name]
	if o == false {
		panic("expected comparison: " + b.Op.Name)
	}
	x, y := ev(b.X), ev(b.Y)
	return x, y, op
}
func swtch(s wg.Switch) { // computed goto
	e := ev(s.E)
	l := make([]int, len(s.Case))
	v := make([]string, len(s.Case))
	for i := range l {
		l[i] = 10 * (1 + len(lab))
		v[i] = strconv.Itoa(l[i])
		lab["switch#"+v[i]] = l[i]
	}
	le := 10 * (1 + len(lab))
	lab["switch#"+strconv.Itoa(le)] = le
	je := fmt.Sprintf("GOTO %d\n", le)

	fmt.Fprintf(w, "GOTO(%s), 1+%s\n", strings.Join(v, ","), e)
	if s.Def != nil {
		emit(s.Def)
		fmt.Fprint(w, je)
	}
	for i := range s.Case {
		fmt.Fprintf(w, ":%d:CONTINUE\n", l[i])
		emit(s.Case[i])
		fmt.Fprint(w, je)
	}
	fmt.Fprintf(w, ":%d:CONTINUE\n", le)
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

	if f.Simple == true {
		l := f.Body[len(f.Body)-1]
		if b, o := l.(wg.Branch); o && b.Break == false {
			f.Body = f.Body[:len(f.Body)-1] // remove last continue
		}
	}

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
	fmt.Fprintf(w, "GOTO %d\n", l)
	if f.Cond != nil {
		fmt.Fprintf(w, "ENDIF\n")
	}
	//fmt.Fprintf(w, "GOTO %d\n", l+1)      // loop next
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
		if bytes.HasPrefix(v[i], []byte("GOTO(")) {
			a := bytes.IndexByte(v[i], ')')
			x := strings.Split(string(v[i][5:a]), ",")
			for i := range x {
				m[atoi(x[i])] = true
			}
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
	b = bytes.ReplaceAll(b, []byte("RETURN\nRETURN\n"), []byte("RETURN\n"))
	v := bytes.Split(b, []byte{10})
	l := 6
	for i := range v {
		s := string(v[i])
		if strings.HasPrefix(s, "ENDIF") {
			l--
		}
		if len(s) > 0 && s[0] == ':' { // :label:...
			s = s[1:]
			p := strings.Index(s, ":")
			l := atoi(s[:p])
			v[i] = []byte(fmt.Sprintf("%-6d%s", l, s[1+p:]))
		} else {
			if l > 0 { // l<=0 should not happen
				v[i] = append(bytes.Repeat([]byte(" "), l), v[i]...)
			}
		}
		r = append(r, wrap77(v[i])...)
		r = append(r, 10)

		if strings.HasPrefix(s, "IF") && strings.HasSuffix(s, "THEN") {
			l++
		}
	}
	return r
}
func wrap77(s []byte) (r []byte) {
	if len(s) <= 72 {
		return s
	}
	r = append(r, s[:72]...)
	s = s[72:]
	for {
		r = append(r, []byte("\n     +")...)
		if len(s) <= 66 {
			r = append(r, s...)
			return r
		}
		r = append(r, s[:66]...)
		s = s[66:]
	}
	return r
}
func ssa(t wg.Type) string { // create new ssa variable
	q := newsym()
	s := addsym(q, _loc, loc_)
	push(s)
	typ[s] = t
	return s
}
func newsym() string {
	all := make(map[string]bool)
	for _, a := range CUR.Args {
		all[a.Name] = true
	}
	for _, a := range CUR.Locs {
		all[a.Name] = true
	}
	for i := 'a'; i < 'z'; i++ {
		if all[string(i)] == false {
			return string(i)
		}
	}
	panic("no new symbol")
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
	//if simple(f) == false {
	for i := range f.Rets {
		n := newname()
		s := addsym(n, _loc, loc_)
		typ[s] = f.Rets[i]
		ret[i] = n
	}
	//}
}
func assignLocs(f wg.Func) {
	// init locals to 0, if they are used by localget and are not assigned first.
	m := make(map[string]bool)
	for _, s := range f.Locs {
		s := loc(s.Name)
		m[s] = get[s]
	}
	st := f.Body
	if len(st) > 0 {
		if l, o := st[0].(wg.Stmts); o {
			st = l
		}
		for _, e := range st {
			a, o := e.(wg.Assign)
			if o {
				for i := range a.Name {
					if a.Glob[i] == false {
						m[loc(a.Name[i])] = false
					}
				}
			}
			if o == false { // only check first assign statements
				break
			}
		}
	}
	for s, o := range m {
		if o {
			fmt.Fprintf(w, "%s = 0\n", s)
		}
	}
}
func newname() string { // new local name not in loc_
	s := "r"
	if _, o := loc_[s]; o == false {
		return s
	}
	for i := 0; ; i++ {
		s = "r" + strconv.Itoa(i)
		if _, o := loc_[s]; o == false {
			return s
		}
	}
}
func addsym(s string, _m map[string]string, m_ map[string]string) string {
	v := strings.ToUpper(s)
	v = strings.ReplaceAll(v, ".", "")
	v = strings.ReplaceAll(v, ":", "")
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
			c = v[:6-len(p)] + p
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
func declare(d map[string][]string) {
	var t []string
	for k := range d {
		t = append(t, k)
	}
	sort.Strings(t)
	for _, tp := range t {
		v := d[tp]
		sort.Strings(v)
		fmt.Fprintf(w, "%s %s\n", tp, strings.Join(uniq(v), ","))
	}
}
func uniq(v []string) (r []string) {
	m := make(map[string]bool)
	r = make([]string, 0, len(v))
	for _, s := range v {
		if m[s] == false {
			m[s] = true
			r = append(r, s)
		}
	}
	return r
}
func commons(v []string) {
	for _, s := range v {
		fmt.Fprintf(w, "COMMON /%s/ %s\n", s, s)
	}
}
func sigi(arg, res []wg.Type) (t string) { // indirect function call signature "i:jj"
	tp := func(t wg.Type) string {
		switch t {
		case wg.I32, wg.U32, wg.I8x16, wg.I32x4:
			return "i"
		case wg.I64, wg.U64:
			return "j"
		case wg.F64, wg.F64x2:
			return "f"
		default:
			panic(fmt.Sprintf("ftab: unknown type: %T", t))
		}
	}
	for i := range res {
		t += tp(res[i])
	}
	t += ":"
	for i := range arg {
		t += tp(arg[i])
	}
	return t
}
func sigif(f wg.Func) string {
	a := make([]wg.Type, 0)
	for i := range f.Args {
		a = append(a, f.Args[i].Type)
	}
	return sigi(a, f.Rets)
}
func ftab(mod wg.Module) ([]byte, []byte) { // one dispatch function per signature, each with a jump table
	max := 0
	m := make(map[string][]int)     // sig->ints
	g := make(map[int]int)          // func index to group index(by sig)
	n := make(map[string]wg.Func)   // last function with sig
	itab := make(map[int]string)    // flat table index to f.Name
	sigt := make(map[string]string) // f.Name -> sig
	for _, e := range mod.Table {
		for i, s := range e.Names {
			fn, o := fun[s]
			if o == false {
				panic("ftab: unknown func:" + s)
			}
			j := e.Off + i
			if j > max {
				max = j
			}
			//s := addsym(sigif(fn), _sym, sym_)
			//reserved[s] = true
			s := sigif(fn)
			m[s] = append(m[s], j)
			g[j] = len(m[s]) - 1
			n[s] = fn
			itab[j] = fn.Name
			sigt[fn.Name] = s
		}
	}
	v := make([]string, 0, len(m))
	for i := range m {
		v = append(v, i)
	}

	var buf bytes.Buffer
	w_ := w
	w = &buf
	sort.Strings(v)

	d := fmt.Sprintf("INTEGER*4 FNC(%d)\nCOMMON /FNC/FNC\n", 1+max)
	{
		v := make([]string, 1+max)
		for i := 0; i <= max; i++ {
			v[i] = "0"
			if j, o := g[i]; o {
				v[i] = strconv.Itoa(1 + j)
			}
		}
		d += "FNC = (/" + strings.Join(v, ",") + "/)\n"
	}

	for _, u := range v {
		//dispatch(mod, u, m[u], n[u], max, itab, sigt)
		x := m[u]
		names := make([]string, len(x))
		for i := range x {
			names[i] = itab[x[i]]
		}
		dispatch(u, x, names, n[u], max)
	}
	w = w_
	return []byte(d), buf.Bytes()
}

//func dispatch(mod wg.Module, sig string, v []int, f wg.Func, max int, itab map[int]string, sigt map[string]string) { // emit dispatch function for sig
func dispatch(sig string, v []int, names []string, f wg.Func, max int) {
	s := addsym(sig, _sym, sym_)
	reserved[s] = true
	tab[sig] = s
	args := "ABCDEFGHIJKLM"
	args = args[:len(f.Args)]
	rets := "UVWXYZ"
	rets = rets[len(rets)-len(f.Rets):]
	simple := len(f.Rets) == 1 && len(f.Args) == 1
	var arglist []string
	d := make(map[string][]string)
	if simple {
		args = "X"
		rets = ""
		rt := t77(f.Rets[0])
		fmt.Fprintf(w, "%s FUNCTION %s(X,N)\n", rt, s)
		t := t77(f.Args[0].Type)
		d[t] = append(d[t], "X")
		for i := range names {
			d[rt] = append(d[rt], sym(names[i]))
		}
	} else {
		l := strings.Split(args, "")
		l = append(l, strings.Split(rets, "")...)
		l = append(l, "N")
		arglist = l
		a := strings.Join(l, ",")
		fmt.Fprintf(w, "SUBROUTINE %s(%s)\n", s, a)
		for i, s := range args {
			t := t77(f.Args[i].Type)
			d[t] = append(d[t], string(s))
		}
		for i, s := range rets {
			t := t77(f.Rets[i])
			d[t] = append(d[t], string(s))
		}
	}
	d["INTEGER*4"] = append(d["INTEGER*4"], "N")
	declare(d)
	fmt.Fprintf(w, "INTEGER*4 FNC(%d)\n", 1+max)
	fmt.Fprintf(w, "COMMON /FNC/FNC\n")
	jota := make([]string, len(v))
	for i := range jota {
		jota[i] = strconv.Itoa(1 + i)
	}
	fmt.Fprintf(w, "GOTO(%s),FNC(1+N)\n", strings.Join(jota, ","))
	fmt.Fprintf(w, "WRITE(*,*)'DISPATCH FAILED',N,FNC(1+N)\n")
	var a string
	if simple == false {
		a = strings.Join(arglist[:len(arglist)-1], ",")
	}
	for i := range v {
		if simple {
			fmt.Fprintf(w, ":%d:%s = %s(X)\nRETURN\n", 1+i, s, sym(names[i]))
		} else {
			fmt.Fprintf(w, ":%d:CALL %s(%s)\nRETURN\n", 1+i, sym(names[i]), a)
		}
	}
	fmt.Fprintf(w, "RETURN\nEND\n")
}
func atoi(s string) int {
	i, e := strconv.Atoi(s)
	if e != nil && strings.HasPrefix(s, "0x") {
		var i64 int64
		i64, e = strconv.ParseInt(s[2:], 16, 32)
		i = int(i64)
	}
	if e != nil {
		panic("expected integer: " + s)
	}
	return i
}
func memsize(s string) string {
	n := MEMSIZE
	s = strings.ReplaceAll(s, "#1", strconv.Itoa(n))
	s = strings.ReplaceAll(s, "#4", strconv.Itoa(n/4))
	s = strings.ReplaceAll(s, "#8", strconv.Itoa(n/8))
	s = strings.ReplaceAll(s, "#z", strconv.Itoa(n/16))
	return s
}
func blockdata(m wg.Module) {
	if len(m.Data) == 0 {
		return
	}
	// store all data segments as []int32
	max := 0
	for _, d := range m.Data {
		i := d.Off + len(d.Data)
		if i > max {
			max = i
		}
	}
	if 4*(max/4) < max {
		max = 4 * (1 + max/4)
	}
	b := make([]byte, max)
	for _, d := range m.Data {
		copy(b[d.Off:], []byte(d.Data))
	}
	j := make([]int32, max/4)
	if e := bin.Read(bytes.NewReader(b), bin.LittleEndian, &j); e != nil {
		panic(e)
	}

	fmt.Fprintf(w, memsize(block))
	v := make([]string, len(j))
	for i := range j {
		v[i] = strconv.FormatInt(int64(j[i]), 10)
		// handle minint32 if exists
	}
	s := strings.Join(v, ",")
	z := (MEMSIZE - 4*len(j)) / 4
	fmt.Fprintf(w, "%s,%d*0/\nEND\n", s, z)
}

const block = `BLOCK DATA
INTEGER*1 I8(#1)
INTEGER*4 I(#4)
EQUIVALENCE(I8,I)
COMMON /MEM/I8
DATA I/`

const fhead = `PROGRAM MAIN
IMPLICIT NONE
INTEGER *4 NALLOC
COMMON /NALLOC/NALLOC
`

/*
const mem = `INTEGER*1 I8(#1)
INTEGER*4 I32(#4)
INTEGER*8 I64(#8)
REAL*8    F64(#8)
COMMON /MEM/I8
EQUIVALENCE(I8,I32,I64,F64)
`
*/

const builtins = `INTEGER*4 FUNCTION IB(X)
LOGICAL X
IB = 0
IF(X) IB = 1
RETURN
END

REAL*8 FUNCTION FCASTI(X)
INTEGER*8 X, Y
REAL*8    F
EQUIVALENCE(Y,F)
Y = X
FCASTI = F
RETURN
END

INTEGER*8 FUNCTION ICASTF(X)
INTEGER*8 R
REAL*8    X, Y
EQUIVALENCE(Y,R)
Y = X
ICASTF = R
RETURN
END

INTEGER*4 FUNCTION XWRITE(F,N,S,M)
INTEGER*4 F,N,S,M,I,Q
INTEGER*1 I8(#1)
CHARACTER B(#1)
CHARACTER(512) C
COMMON /MEM/I8
EQUIVALENCE(I8,B)
IF(N.NE.0)THEN
DO I=1,N
C(I:I)=B(F+I)
ENDDO
C = C(1:N)
OPEN(11,FILE=C,STATUS="REPLACE",ACCESS="STREAM",ACTION="WRITE")
WRITE(11) I8(1+S:S+M)
CLOSE(UNIT=11)
ELSE
DO I=1,M
 Q = FPUT(B(S+M))
ENDDO
ENDIF
XWRITE=0
RETURN
END

INTEGER*4 FUNCTION XREAD(F,N,D)
INTEGER*4 F,N,D,I,NF
INTEGER*1 I8(#1)
CHARACTER B(#1)
CHARACTER(512) C
COMMON /MEM/I8
EQUIVALENCE(I8,B)

DO I=1,N
C(I:I) = B(F+I)
ENDDO
C = C(1:N)

OPEN(11,FILE=C,STATUS="OLD",ACCESS="STREAM",ACTION="READ")
INQUIRE(FILE=C, SIZE=NF)

IF(D .EQ. 0)THEN
XREAD=NF
CLOSE(UNIT=11)
RETURN
ENDIF

READ(11) I8(1+D:D+NF)
CLOSE(UNIT=11)
XREAD = 0
RETURN
END

INTEGER*4 FUNCTION XARG(I,R)
INTEGER*4 I,R
CHARACTER S(512)
INTEGER*1 J(512)
INTEGER*1 I8(#1)
COMMON /MEM/I8
EQUIVALENCE(S,J)
CALL GETARG(I,S)
XARG = 0
IF(R .EQ. 0)THEN
XARG = LEN(S)
RETURN
ENDIF
I8(1+R:R+LEN(S)) = J(1:LEN(S))
RETURN
END

INTEGER*4 FUNCTION XREADI(D,N)
IMPLICIT NONE
CHARACTER C
INTEGER*4 I,S,D,N
INTEGER*1 I8(#1)
CHARACTER B(#1)
EQUIVALENCE(I8,B)
COMMON /MEM/I8
DO I=1,N
S = FGET(C)
IF(S.NE.0)THEN
XREADI = 0
RETURN
ENDIF
B(D+I) = C
IF(I8(D+I) .EQ. 10)THEN
XREADI = I-1
RETURN
ENDIF
ENDDO
XREADI = N
RETURN
END

INTEGER *4 FUNCTION MEMSIZ()
INTEGER *4 NALLOC
COMMON /NALLOC/NALLOC
MEMSIZ = NALLOC
RETURN
END

INTEGER *4 FUNCTION MEMGRW(N64K)
INTEGER *4 NALLOC, N64K
COMMON /NALLOC/NALLOC
WRITE(*,*) "memgrw", N64K
MEMSIZ = NALLOC
NALLOC = NALLOC + N64K
IF(NALLOC .GT. SHIFTR(#1,16))THEN
WRITE(*,*) "WSFULL"
CALL EXIT(1)
ENDIF
RETURN
END
`

/*
PURE FUNCTION Copy_a2s(a)  RESULT (s)    ! copy char array to string
    CHARACTER,INTENT(IN) :: a(:)
    CHARACTER(SIZE(a)) :: s
    INTEGER :: i
    DO i = 1,SIZE(a)
       s(i:i) = a(i)
    END DO
END FUNCTION Copy_a2s
*/
