package wg

import (
	"bytes"
	"encoding/binary"
	"encoding/hex"
	"fmt"
	"io"
	"math"
	"sort"
	"strconv"
	"strings"
)

func (m Module) K(w io.Writer) {
	const na int = -1 << 31
	var C []uint64
	var D []byte
	var T []string
	var P []int
	var I []int
	var S []string
	push := func(t string, p int, i int, s string) int {
		T = append(T, t)
		P = append(P, p)
		I = append(I, i)
		S = append(S, s)
		return len(T) - 1
	}
	typ := map[Type]string{
		I32: "i",
		U32: "I",
		I64: "j",
		U64: "J",
		F64: "f",
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
			return data(math.Float64bits(f))
		default:
			panic(fmt.Sprintf("literal type nyi: %v", l.Type))
		}
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
			p = push("asn", p, na, v.Mod)
			for i, s := range v.Name {
				n := push("sym", p, na, s)
				push("typ", p, na, typ[v.Typs[i]])
				if len(v.Expr) > i { // e.g. a, b := f(x)
					node(v.Expr[i], n)
				}
			}
		case Call:
			p = push("call", p, na, v.Func) // later: na->func node
			nodes(v.Args, p)
		case CallIndirect:
			p = push("cali", p, na, "")
			node(v.Func, p)
			for _, a := range v.Args {
				node(a, p)
			}
			for i, t := range v.ArgType {
				push("arg", p, i, typ[t])
			}
			for i, t := range v.ResType {
				push("res", p, i, typ[t])
			}
		case Cast:
			p = push("cst", p, na, typ[v.Dst])
			push("typ", p, na, typ[v.Src])
			node(v.Arg, p)
		case Drop:
			p = push("drp", p, na, "")
			node(v.Expr, p)
		case Nop:
			p = push("nop", p, na, "")
		case Unary:
			p = push(v.Op.Name, p, 1, typ[v.Op.Type])
			node(v.X, p)
		case Binary:
			p = push(v.Op.Name, p, 2, typ[v.Op.Type])
			node(v.X, p)
			node(v.Y, p)
		case Literal:
			push("lit", p, literal(v), typ[v.Type])
		case GlobalGet:
			push("Get", p, na, string(v)) // later: Get->get na->glo
		case LocalGet:
			push("get", p, na, string(v)) // later: na->declaration node Arg|Loc
		case GlobalGets:
			p = push("Gets", p, na, "")
			for i := range v {
				node(GlobalGet(v[i]), p)
			}
		case LocalGets:
			p = push("gets", p, na, "")
			for i := range v {
				node(LocalGet(v[i]), p)
			}
		case Return:
			p = push("ret", p, na, "")
			nodes(v, p)
		case Stmts:
			p = push("stmts", p, na, "")
			for i := range v {
				node(v[i], p)
			}
		case If:
			p = push("if", p, na, "")
			node(v.If, p)
			node(v.Then, p)
			if len(v.Else) > 0 {
				node(v.Else, p)
			}
		case Switch:
			def := 0
			if len(v.Def) > 0 {
				def = 1
			}
			p = push("swc", p, def, "")
			node(v.E, p)
			for _, n := range v.Case {
				node(n, p)
			}
			if len(v.Def) > 0 {
				node(v.Def, p)
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

	root := push("", na, 0, "")

	// Memory
	if m.Memory != "" {
		push("mem", root, atoi(m.Memory), "")
		if m.Memory2 != "" {
			push("mem", root, atoi(m.Memory2), "")
		}
	}

	// Imports
	var v []string
	for k := range m.Imports {
		v = append(v, k)
	}
	sort.Strings(v)
	if len(v) > 0 {
		imports := push("imports", root, na, "")
		for _, k := range v {
			i := m.Imports[k]
			p := push("import", imports, na, k)
			push("pkg", p, na, i.Package)
			push("sym", p, na, i.Func)
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
			g := push(v, root, na, s)
			node(a.Expr[i].(Literal), g)
		}
	}

	// Table
	for _, te := range m.Table {
		t := push("tab", root, na, "")
		for j, s := range te.Names {
			push("te", t, te.Off+j, s)
		}
	}

	// Data
	for _, d := range m.Data {
		p := push("dat", root, len(D), "")
		push("dat", p, len(d.Data), "")
		D = append(D, []byte(d.Data)...)
	}

	// Funcs
	for i, f := range m.Funcs {
		p := push("fn", root, i, f.Name)
		for i, a := range f.Args {
			ai := push("arg", p, i, typ[a.Type])
			push("sym", ai, i, a.Name)
		}
		for i, r := range f.Rets {
			push("res", p, i, typ[r])
		}
		for i, l := range f.Locs {
			li := push("loc", p, i, typ[l.Type])
			push("sym", li, i, l.Name)
		}
		b := push("ast", p, na, "")
		node(f.Body, b)
		if f.Defer != nil {
			d := push("defer", p, na, "")
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
}
