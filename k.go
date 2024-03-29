package wg

import (
	"bytes"
	"encoding/binary"
	"encoding/hex"
	"fmt"
	"go/ast"
	"io"
	"math"
	"sort"
	"strconv"
	"strings"
)

func (m Module) K(w io.Writer) {
	var current Func
	const na int = -1 << 31
	var C []uint64
	var D []byte
	var T []string
	var P []int
	var I []int
	var S []string
	var rtyp string
	mcalls := make(map[string]bool)
	push := func(t string, p int, i int, s string) int {
		T = append(T, t)
		P = append(P, p)
		I = append(I, i)
		S = append(S, s)
		return len(T) - 1
	}
	typ := map[Type]string{
		I32:   "i",
		U32:   "u",
		I64:   "j",
		U64:   "k",
		F64:   "f",
		I8x16: "C",
		I32x4: "I",
		F64x2: "F",
	}
	var vars map[string]bool
	un := func(s string) string {
		r := map[string]string{"-": "neg", "!": "not"}[s]
		if r == "" {
			panic(fmt.Sprintf("unary %q nyi", s))
		}
		return r
	}
	bi := func(s string) string {
		r := map[string]string{"==": "eql", "<": "les", ">": "mor", ">=": "gte", "<=": "lte", "&&": "bnd", "&": "and", "||": "bor", "|": "orr", "^": "xor", "!=": "neq", "&^": "ant", "-": "sub", "+": "add", "*": "mul", "/": "div", "%": "mod", ">>": "shr", "<<": "shl"}[s]
		if r == "" {
			panic(fmt.Sprintf("binary %q nyi", s))
		}
		return r
	}
	atoi := func(s string) int {
		i, e := strconv.Atoi(s)
		if e != nil {
			panic(e)
		}
		return i
	}
	syms := func(v []string) (r string) {
		var w strings.Builder
		a := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
		n := a + "0123456789"
		for _, s := range v {
			if len(s) > 0 {
				if strings.IndexByte(a, s[0]) < 0 {
					s = `"` + s + `"`
				} else {
					for _, c := range s[1:] {
						if strings.ContainsRune(n, c) == false {
							s = `"` + s + `"`
							break
						}
					}
				}
			}
			w.WriteByte('`')
			w.WriteString(s)
		}
		return w.String()
	}
	ints := func(v []int) (r string) {
		var w strings.Builder
		for j, i := range v {
			if j > 0 {
				w.WriteByte(32)
			}
			if i == na {
				w.WriteString("0N")
			} else {
				w.WriteString(strconv.Itoa(i))
			}
		}
		return w.String()
	}
	data := func(u uint64) int {
		for i, d := range C {
			if d == u {
				return 8 * i
			}
		}
		C = append(C, u)
		return 8 * (len(C) - 1)
	}
	literal := func(l Literal) int {
		switch l.Type {
		case I32, U32:
			if strings.HasPrefix(l.Value, "0x") {
				i, e := strconv.ParseUint(l.Value[2:], 16, 32)
				fatal(e)
				return int(i)
			} else {
				if l.Type == U32 {
					i, e := strconv.ParseUint(l.Value, 10, 32)
					fatal(e)
					return int(int32(i))
				}
				i, e := strconv.ParseInt(l.Value, 10, 32)
				fatal(e)
				return int(i)
			}
		case I64, U64:
			if strings.HasPrefix(l.Value, "0x") {
				i, e := strconv.ParseUint(l.Value[2:], 16, 64)
				fatal(e)
				return data(uint64(i))
			} else {
				i, e := strconv.ParseInt(l.Value, 10, 64)
				fatal(e)
				return data(uint64(i))
			}
		case F64:
			f, e := strconv.ParseFloat(l.Value, 64)
			fatal(e)
			r := data(math.Float64bits(f))
			return r
		default:
			panic(fmt.Sprintf("literal type nyi: %v", l.Type))
		}
	}
	ib := func(b bool) int {
		if b {
			return 1
		}
		return 0
	}
	sy := func(s string) string { return strings.ReplaceAll(s, ".", "") }
	sym := func(s string) string { // sym(
		s = sy(s)
		if vars[s] {
			panic("var " + s + " already exists")
		}
		vars[s] = true
		return s
	}
	var node func(e Emitter, p int)
	nodes := func(v []Expr, p int) {
		for i := range v {
			node(v[i], p)
		}
	}
	node = func(e Emitter, p int) {
		switch v := e.(type) {
		case Assign:
			if len(v.Expr) == 0 {
				return // declarations
			}
			mod := strings.TrimSuffix(v.Mod, "=")
			if mod == ":" {
				mod = ""
			}
			if mod != "" { // modified assignment: 1 symbol, 1 expr
				glo := ib(v.Glob[0])
				s := sy(v.Name[0])
				p = push("asn", p, glo, s)
				p = push(bi(mod), p, 2, typ[v.Typs[0]])
				if glo == 1 {
					push("Get", p, na, s)
				} else {
					push("get", p, na, s)
				}
				node(v.Expr[0], p)
			} else if len(v.Expr) > 1 { // multiple expressions n times: 1 to 1 assignment
				p = push("stm", p, na, "")
				for i, e := range v.Expr {
					p1 := push("asn", p, ib(v.Glob[i]), sy(v.Name[i]))
					node(e, p1)
				}
			} else { // possibly multiple return values: n symbols <- 1 expr
				if len(v.Name) != 1 {
					panic("multi-assign")
				}
				p = push("asn", p, ib(v.Glob[0]), sy(v.Name[0]))
				node(v.Expr[0], p)
			}
		case Call:
			m := map[string]string{"I8": "b", "I32": "i", "I64": "j", "F64": "f", "SetI8": "b", "SetI32": "i", "SetI64": "j", "SetF64": "f"}
			t := m[v.Func]
			if t == "" {
				p = push("cal", p, na, strings.ReplaceAll(v.Func, ".", "")) // later: na->func node
			} else {
				if strings.HasPrefix(v.Func, "Set") {
					p = push("sto", p, na, t)
				} else {
					p = push("lod", p, na, t)
				}
			}
			x := len(P)
			nodes(v.Args, p)
			if t == "" && len(v.Args) > 1 {
				if s := mcall(v.a, T[x:], S[x:]); s != "" {
					mcalls[s] = true
				}
			}
		case CallIndirect:
			rt := ""
			if len(v.ResType) == 1 {
				rt = typ[v.ResType[0]]
			}
			p = push("cli", p, len(v.Args), rt)
			node(v.Func, p)
			for _, a := range v.Args {
				node(a, p)
			}
			x := len(P)
			for _, t := range v.ArgType {
				push("arg", p, na, typ[t])
			}
			if len(v.ArgType) > 1 {
				if s := mcall(v.a, T[x:], S[x:]); s != "" {
					mcalls[s+"(indirect)"] = true
				}
			}
		case Cast:
			p = push("cst", p, na, typ[v.Dst])
			push("typ", p, na, typ[v.Src])
			node(v.Arg, p)
		case Drop:
			p = push("drp", p, na, "")
			node(v.Expr.(Call), p) // all children are calls
		case Nop:
			p = push("nop", p, na, "")
		case Unary:
			p = push(un(v.Op.Name), p, 1, typ[v.Op.Type])
			node(v.X, p)
		case Binary:
			p = push(bi(v.Op.Name), p, 2, typ[v.Op.Type])
			node(v.X, p)
			node(v.Y, p)
		case Literal:
			push("lit", p, literal(v), typ[v.Type])
		case GlobalGet:
			push("Get", p, na, string(v))
		case LocalGet:
			s := sy(string(v))
			push("get", p, na, s)
		case GlobalGets:
			for i := range v {
				node(GlobalGet(v[i]), p)
			}
		case LocalGets:
			for i := range v {
				node(LocalGet(v[i]), p)
			}
		case Return:
			p = push("ret", p, na, rtyp)
			nodes(v, p)
		case Stmts:
			if len(v) == 1 {
				node(v[0], p)
			} else {
				p = push("stm", p, na, "")
				for i := range v {
					node(v[i], p)
				}
			}
		case If:
			th, el, name, t, ret := v.value()
			if ret {
				t = current.Rets[0]
			}
			if t != "" {
				if ret {
					p = push("ret", p, na, typ[t])
				} else {
					p = push("asn", p, 0, name) //todo global asn
				}
			}
			p = push("cnd", p, na, typ[t])
			node(v.If, p)
			node(th, p)
			if len(el) > 0 {
				node(el, p)
			}
		case Switch:
			cs, de, name, t, ret := v.value()
			if ret {
				t = current.Rets[0]
			}
			if t != "" {
				if ret {
					p = push("ret", p, na, typ[t])
				} else {
					p = push("asn", p, 0, name) //todo global asn
				}
			}
			if len(de) == 0 {
				panic("switch no-default?")
			}
			p = push("swc", p, ib(len(de) > 0), typ[t])
			node(v.E, p)
			for _, n := range cs {
				node(n, p)
			}
			if len(de) > 0 {
				node(de, p)
			}
		case For:
			simple := 0
			if v.Simple {
				simple = 1
			}
			p = push("for", p, simple, v.Label)
			if v.Cond != nil {
				node(v.Cond, p)
			} else {
				node(Nop{}, p)
			}
			if v.Post != nil {
				node(v.Post, p)
			} else {
				node(Nop{}, p)
			}
			node(v.Body, p)
		case Branch:
			brk := 0
			if v.Break {
				brk = 1
			}
			p = push("jmp", p, brk, v.Label)
		default:
			panic(fmt.Sprintf("node %T nyi", e))
		}
	}

	root := push("prg", 0, 0, m.Package)

	// Memory
	if m.Memory != "" {
		push("mem", root, atoi(m.Memory), "a")
		if m.Memory2 != "" {
			push("mem", root, atoi(m.Memory2), "b")
		}
	}

	// Imports
	var v []string
	for k := range m.Imports {
		v = append(v, k)
	}
	sort.Strings(v)
	if len(v) > 0 {
		for _, k := range v {
			i := m.Imports[k]
			p := push("imp", root, na, k)
			push("pkg", p, na, i.Package)
			push("sym", p, na, sy(i.Func))
			for _, a := range i.Arg {
				push("arg", p, na, typ[a])
			}
			for _, a := range i.Res {
				push("res", p, na, typ[a])
			}
		}
	}

	// Globals
	for _, a := range m.Globals {
		n := len(a.Name)
		for i, s := range a.Name {
			if len(a.Expr) != n {
				panic("global #expr")
			}
			v := "var"
			if a.Const[i] {
				v = "con"
			}
			t := a.Typs[i]
			g := push(v, root, na, s)
			l := a.Expr[i].(Literal)
			l.Type = t // overwrite literal type (which maybe wrong)
			node(l, g)
		}
	}

	// Table
	for _, te := range m.Table {
		for j, s := range te.Names {
			push("tab", root, te.Off+j, s)
		}
	}

	// Data
	for _, d := range m.Data {
		if n := d.Off - len(D); n < 0 {
			panic("overlapping data")
		} else {
			D = append(D, bytes.Repeat([]byte{0}, n)...)
		}
		D = append(D, []byte(d.Data)...)
	}

	// Funcs
	for _, f := range m.Funcs {
		current = f
		f.Exported = m.Exports[f.Name] || m.exportAll
		p := push("fun", root, ib(f.Exported), f.Name)
		for _, a := range f.Args {
			ai := push("arg", p, na, typ[a.Type])
			push("sym", ai, 0, sy(a.Name))
		}
		for _, r := range f.Rets {
			push("res", p, na, typ[r])
		}
		vars = make(map[string]bool)
		for _, l := range f.Locs {
			li := push("loc", p, na, typ[l.Type])
			s := sym(l.Name)
			push("sym", li, 0, s)
		}
		rtyp = ""
		if len(f.Rets) == 1 {
			rtyp = typ[f.Rets[0]]
		}
		b := push("ast", p, na, "")
		for i := range f.Body {
			node(f.Body[i], b)
		}
		if f.Defer != nil {
			d := push("dfr", p, na, "")
			node(f.Defer.(Call), d)
		}
	}

	// emit
	var con bytes.Buffer
	fatal(binary.Write(&con, binary.LittleEndian, C))

	fmt.Fprintf(w, "C:0x%s\n", hex.EncodeToString(con.Bytes()))
	fmt.Fprintf(w, "D:0x%s\n", hex.EncodeToString(D))
	fmt.Fprintf(w, "T:%s\n", syms(T))
	fmt.Fprintf(w, "P:%s\n", ints(P))
	fmt.Fprintf(w, "I:%s\n", ints(I))
	fmt.Fprintf(w, "S:%s\n", syms(S))

	/* print calls with subcalls that reference variables multiple times
	keys := make([]string, 0)
	for s := range mcalls {
		keys = append(keys, s)
	}
	sort.Strings(keys)
	for _, s := range keys {
		fmt.Println("mcall", s)
	}
	*/
}
func mcall(a ast.Node, T, S []string) string { //function arguments do 2 or more calls, and reference variables multiple times.
	l := make(map[string]int)
	n := 0
	for i, t := range T {
		if t == "cal" || t == "cli" {
			n++
		}
		if t == "get" {
			l[S[i]]++
		}
	}
	if n > 1 {
		for _, v := range l {
			if v > 1 {
				return position(a)
			}
		}
	}
	return ""
}
