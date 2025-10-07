package wg

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

func (m Module) A(w io.Writer) {
	O := func(s string) int { w.Write([]byte(s + "\n")); return 0 }
	I := func(a, b, c string) { w.Write([]byte(" " + a + " " + b + "," + c + "\n")) }
	I1 := func(a, b string) { w.Write([]byte(" " + a + " " + b + "\n")) }
	ri := func(i int) string {
		if i < 0 {
			return "#" + strconv.Itoa(-i-1)
		} else {
			return "$" + strconv.Itoa(i)
		}
	}
	com := func(s string) string { return " ;" + s }
	var tmp int
	var loc map[string]int
	var col map[int]string
	labels := make(map[string]int)
	lab := func(s string) string {
		i := labels[s]
		labels[s] = 1 + i
		return "." + s + strconv.Itoa(i)
	}
	cmp := func(op string, neg bool) string {
		a := []string{">", ">=", "==", "!=", "<=", "<"}
		b := []string{"g", "ge", "e", "ne", "le", "l"}
		c := []string{"le", "l", "ne", "e", "g", "ge"}
		if neg {
			b = c
		}
		for i, o := range a {
			if o == op {
				return "j" + b[i]
			}
		}
		panic("cmp: " + op)
	}

	ops := map[string]string{
		"+": "add", "*": "imul", "-": "sub",
	}
	aso := func(x string) bool { return x == "+" || x == "*" }
	alloc := func() (r int) { r = tmp; tmp++; return r }

	var node func(e Emitter) int
	node = func(e Emitter) int {
		switch v := e.(type) {
		case Stmts:
			for i, x := range v {
				node(x)
				if i < len(v)-1 {
					O("")
				}
			}
		case Drop:
			node(v.Expr)
			return -1
		case Nop:
			return -1
		case Return:
			if len(v) == 1 {
				x := node(v[0])
				if x != 0 {
					I("mov", ri(0), ri(x))
				}
			} else if len(v) != 0 {
				panic("multi-return")
			}
			return O(" ret")
		case Binary:
			x := node(v.X)
			y := node(v.Y)
			s := ops[v.Op.Name]
			if s == "" {
				s = "op(" + v.Op.Name + ")"
			}
			r := alloc()
			if r != x {
				if r == y && aso(v.Op.Name) {
					x, y = y, x
				} else {
					I("mov", ri(r), ri(x))
				}
			}
			I(s, ri(r), ri(y))
			return r
		case Unary:
			x := node(v.X)
			r := alloc()
			s := []string{"neg"}[strings.Index("-", v.Op.Name)]
			if r != x {
				I("mov", ri(r), ri(x))
			}
			I1(s, ri(r))
			return r
		case LocalGet:
			return loc[string(v)]
		case Assign:
			if len(v.Expr) == 0 {
				return -1 // declarations
			}
			mod := strings.TrimSuffix(v.Mod, "=")
			if mod == ":" {
				mod = ""
			}
			if mod != "" { // modified assignment: 1 symbol, 1 expr
				if v.Glob[0] {
					panic("nyi:global-modified-assign")
				}
				if l, o := v.Expr[0].(Literal); o {
					I(ops[mod], ri(loc[v.Name[0]]), l.Value+com(v.Name[0]))
					return -1
				}
				I(ops[mod], ri(loc[v.Name[0]]), ri(node(v.Expr[0]))+com(v.Name[0]))
				return -1
			} else if len(v.Expr) > 1 { // multiple expressions n times: 1 to 1 assignment
				panic("multi-assign")
			}
			if l, o := v.Expr[0].(Literal); o {
				I("mov", ri(loc[v.Name[0]]), l.Value+com(v.Name[0]))
				return -1
			}
			x := node(v.Expr[0])
			r := loc[v.Name[0]]
			if r != x {
				I("mov", ri(r), ri(x)+com(v.Name[0]))
			}
		case Literal:
			i := alloc()
			I("mov", ri(i), v.Value) //v.Type
			return i
		case If:
			b := v.If.(Binary)
			if l, o := b.Y.(Literal); o {
				I("cmp", ri(node(b.X)), l.Value)
			} else {
				I("cmp", ri(node(b.X)), ri(node(b.Y)))
			}
			fi, el := lab("fi"), lab("el")
			l1 := el
			if len(v.Then) == 0 {
				l1 = fi
			}
			I1(cmp(b.Op.Name, false), l1)
			node(v.Then)
			if len(v.Else) > 0 {
				I1("jmp", fi)
				O(el + ":")
				node(v.Else)
			}
			O(l1 + ":")
			return -1
		case Switch:
			c := node(v.E)
			l := make([]string, len(v.Case))
			e := lab("ec")
			for i := range v.Case {
				I("cmp", ri(c), strconv.Itoa(i))
				l[i] = lab("cs")
				I1("je", l[i])
			}
			d := lab("def")
			I1("jmp", d)
			for i, c := range v.Case {
				O(l[i] + ":")
				node(c)
				I1("jmp", e)
			}
			O(d + ":")
			node(v.Def)
			O(e + ":")
			return -1
			/*
				case Call:
					m := map[string]string{"I8": "b", "I32": "i", "I64": "j", "F64": "f", "SetI8": "b", "SetI32": "i", "SetI64": "j", "SetF64": "f"}
					t := m[v.Func]
					if t == "" {
						p = push("cal", p, 0, strings.ReplaceAll(v.Func, ".", "")) // later: na->func node
					} else {
						if strings.HasPrefix(v.Func, "Set") {
							p = push("sto", p, 0, t)
						} else {
							p = push("lod", p, 0, t)
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
						push("arg", p, 0, typ[t])
					}
					if len(v.ArgType) > 1 {
						if s := mcall(v.a, T[x:], S[x:]); s != "" {
							mcalls[s+"(indirect)"] = true
						}
					}
				case Cast:
					p = push("cst", p, 0, typ[v.Dst])
					push("typ", p, 0, typ[v.Src])
					node(v.Arg, p)
				case If:
					th, el, name, t, ret := v.value()
					if ret {
						t = current.Rets[0]
					}
					if t != "" {
						if ret {
							p = push("ret", p, 0, typ[t])
						} else {
							p = push("asn", p, 0, name) //todo global asn
						}
					}
					p = push("cnd", p, 0, typ[t])
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
							p = push("ret", p, 0, typ[t])
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
			*/
		default:
			_ = v
			panic(fmt.Sprintf("node %T nyi", e))
		}
		return -1
	}

	O(";k")
	O("global start")
	O("_start:")
	// Funcs
	for _, f := range m.Funcs {
		loc = make(map[string]int)
		col = make(map[int]string)
		tmp = 0
		labels = make(map[string]int)
		s := ""
		for i, a := range f.Args {
			loc[a.Name] = -1 - i
			col[i] = a.Name
			s += " " + strconv.Itoa(i) + ":" + a.Name
		}
		O(f.Name + ":  ;" + s)
		node(f.Body)
	}
}
