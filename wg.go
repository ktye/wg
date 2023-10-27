package wg

import (
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"io/fs"
	"reflect"
	"strconv"
	"strings"
)

var Small bool
var TryCatch bool
var MultiMemory bool
var fset *token.FileSet
var info types.Info
var pkg *types.Package
var printast bool

func Parse(path string) Module { // file.go or dir
	var f *ast.File
	var e error
	fset = token.NewFileSet()
	if strings.HasSuffix(path, ".go") {
		f, e = parser.ParseFile(fset, path, nil, parser.ParseComments)
		fatal(e)
	} else {
		notest := func(f fs.FileInfo) bool { return !strings.HasSuffix(f.Name(), "_test.go") }
		pkgs, e := parser.ParseDir(fset, path, notest, parser.ParseComments)
		fatal(e)
		if len(pkgs) > 1 {
			panic("multiple packages in " + path)
		}
		for _, p := range pkgs {
			for name, pp := range p.Files {
				com := pp.Comments
				if len(com) > 0 && len(com[0].List) > 0 {
					c := com[0].List[0]
					if c.Text == "//go:build small" {
						if Small == false {
							delete(p.Files, name)
						}
					}
					if c.Text == "//go:build !small" {
						if Small {
							delete(p.Files, name)
						}
					}
				}
			}
			f = ast.MergePackageFiles(p, ast.FilterFuncDuplicates|ast.FilterImportDuplicates)
		}
	}
	//ast.Print(fset, f)
	filterImports(f)

	var conf types.Config
	conf.Importer = importer.For("source", nil) // importer.Default()
	//conf.Importer = importer.Default()
	info = types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
	}
	pkg, e = conf.Check("input", fset, []*ast.File{f}, &info)
	fatal(e)

	if printast {
		ast.Print(fset, f)
	}
	return parseFile(f)
}
func fatal(e error) {
	if e != nil {
		panic(e)
	}
}
func filterImports(a *ast.File) { // the merged package file may have duplicated imports, which the type checker does not like.
	m := make(map[string]bool)
	var decls []ast.Decl
	for j, d := range a.Decls {
		if g, o := d.(*ast.GenDecl); o && g.Tok == token.IMPORT {
			keep := make([]int, 0)
			for i, sp := range g.Specs {
				if s, o := sp.(*ast.ImportSpec); o {
					if p := s.Path.Value; m[p] == false {
						keep = append(keep, i)
						m[p] = true
					}
				}
			}
			r := make([]ast.Spec, len(keep))
			for i := range keep {
				r[i] = g.Specs[i]
			}
			g.Specs = r
			a.Decls[j] = g
		}
		decls = append(decls, d)
	}
	a.Decls = decls
}
func position(a ast.Node) string       { return fset.Position(a.Pos()).String() }
func reflectType(a interface{}) string { return reflect.TypeOf(a).String() }
func parseFile(a *ast.File) (r Module) {
	r.Package = a.Name.Name
	r.Imports = make(map[string]Import)
	r.Exports = make(map[string]bool)
	r.Globals = r.parseGlobals(a)
	r.Funcs = r.parseFuncs(a)
	return r
}
func (m *Module) parseGlobals(a *ast.File) (r []Assign) {
	for _, d := range a.Decls {
		if g, o := d.(*ast.GenDecl); o {
			if g.Tok == token.VAR || g.Tok == token.CONST {
				gs := 1
				if g.Tok == token.CONST {
					gs = 2
				}
				stmts := m.parseDecl(g, gs).(Stmts)
				for _, st := range stmts {
					r = append(r, st.(Assign))
				}
			}
		}
	}
	return r
}
func (m *Module) parseFuncs(a *ast.File) (r []Func) {
	for _, d := range a.Decls {
		if f, o := d.(*ast.FuncDecl); o {
			tf := info.ObjectOf(f.Name).(*types.Func)
			m.scopes = catscopes(tf.Scope())
			ff := m.parseFunc(f)
			if ff.Name != "init" {
				r = append(r, ff)
				//r = append(r, m.parseFunc(f))
			}
		}
	}
	return r
}
func (m *Module) parseFunc(a *ast.FuncDecl) (f Func) {
	m.current = &f
	f.Name = a.Name.Name
	args := a.Type.Params.List
	if a.Recv != nil {
		args = append(a.Recv.List, args...)
		tn := a.Recv.List[0].Type.(*ast.Ident).Name
		f.Name = tn + "." + f.Name // method
	}
	f.Args = parseArgs(args)
	if a.Type.Results != nil {
		r := parseArgs(a.Type.Results.List)
		f.Rets = make([]Type, len(r))
		for i := range r {
			f.Rets[i] = r[i].Type
			if n := r[i].Name; n != "" && strings.HasPrefix(n, ".") == false {
				f.Locs = append(f.Locs, Local{n, r[i].Type})
			}
		}
	}
	f.Body = m.parseBody(a.Body.List)
	f.Doc = a.Doc.Text()
	locs := f.Locs
	f.Locs = nil
	for _, v := range locs {
		if v.Name != "_" {
			f.Locs = append(f.Locs, v)
		}
	}
	return f
}
func parseArgs(a []*ast.Field) (r []Arg) {
	for i := range a {
		r = append(r, parseArgi(a[i])...)
	}
	return r
}
func parseArgi(a *ast.Field) (r []Arg) {
	tn, t := parseTypes(a.Type)
	names := []string{""}
	if len(a.Names) > 0 {
		names = make([]string, len(a.Names))
		for i := range a.Names {
			names[i] = a.Names[i].Name
		}
	}
	for i := range names {
		for j := range tn { // expand structs
			name := names[i]
			if s := tn[j]; s != "" {
				name += "." + s
			}
			r = append(r, Arg{name, Type(t[j])})
		}
	}
	return r
}
func parseTypes(a ast.Expr) (names []string, rtype []Type) {
	pos := position(a)
	var f func(string, types.Type) ([]string, []Type)
	f = func(name string, t types.Type) (names []string, rtype []Type) {
		switch v := t.(type) {
		case *types.Basic:
			return []string{name}, []Type{parseType(t, pos)}
		case *types.Struct:
			for i := 0; i < v.NumFields(); i++ {
				x := v.Field(i)
				n, t := f(x.Name(), x.Type().Underlying())
				if name != "" {
					for i := range n {
						n[i] = name + "." + n[i]
					}
				}
				names = append(names, n...)
				rtype = append(rtype, t...)
			}
			return names, rtype
		case *types.Tuple:
			rtype = parseTupleTypes(v, pos)
			names = make([]string, len(rtype))
			for i := range names {
				names[i] = name
			}
			return names, rtype
		//case *types.Array:
		//	vt := parseSimdType(v, pos)
		//	return []string{name}, []Type{vt}
		default:
			panic(position(a) + ": unknown type: " + reflectType(v))
		}
	}
	return f("", info.TypeOf(a).Underlying())
}

//	func parseSimdType(a *types.Array, pos string) Type {
//		switch a.String() {
//		case "[16]int8":
//			return I8x16
//		case "[4]int32":
//			return I32x4
//		case "[2]float64":
//			return F64x2
//		default:
//			panic(pos + ": unknown vector type: " + a.String())
//		}
//	}
func (m *Module) parseSignature(a ast.Expr) (arg, res []Type) {
	p := position(a)
	t := info.TypeOf(a).Underlying().(*types.Signature)
	return parseTupleTypes(t.Params(), p), parseTupleTypes(t.Results(), p)
}
func parseTupleTypes(t *types.Tuple, pos string) (r []Type) {
	r = make([]Type, t.Len())
	for i := range r {
		r[i] = parseType(t.At(i).Type(), pos)
	}
	return r
}
func parseType(t types.Type, pos string) Type {
	switch v := t.Underlying().(type) {
	case *types.Tuple:
		if v.Len() == 0 {
			return V
		} else {
			panic(pos + ": tuple type with length>0")
		}
	default:
		//fmt.Printf("%T\n", t.Underlying())
	}
	s := t.Underlying().String()

	switch s {
	case "bool", "untyped bool":
		return U32
	case "int32", "untyped int":
		return I32
	case "uint32":
		return U32
	case "int64":
		return I64
	case "uint64":
		return U64
	case "float32":
		return F32
	case "float64", "untyped float":
		return F64
	case "[16]int8":
		return I8x16
	case "[4]int32":
		return I32x4
	case "[2]float64":
		return F64x2
	default:
		panic(pos + ": unknown type: " + s)
	}
}
func (m *Module) parseBody(st []ast.Stmt) (r []Stmt) {
	for i := range st {
		r = append(r, m.parseStmt(st[i], nil))
	}
L:
	for len(r) == 1 {
		if l, o := r[0].(Stmts); o {
			r = l
		} else {
			break L
		}
	}
	return r
}
func (m *Module) parseStmts(v []ast.Stmt) (r Stmts) {
	for i := range v {
		r = append(r, m.parseStmt(v[i], nil))
	}
	return r
}
func (m *Module) parseStmt(st ast.Stmt, f *For) Stmt {
	switch v := st.(type) {
	case *ast.AssignStmt:
		return m.parseAssign(v)
	case *ast.ReturnStmt:
		return m.parseReturn(v)
	case *ast.ExprStmt:
		e := m.parseExpr(v.X)
		if t, o := info.TypeOf(v.X).(*types.Tuple); o {
			for i := 0; i < t.Len(); i++ {
				e = Drop{e}
			}
		} else {
			e = Drop{e}
		}
		return e
	case *ast.DeclStmt:
		return m.parseDecl(v.Decl.(*ast.GenDecl), 0)
	case *ast.IfStmt:
		return m.parseIf(v)
	case *ast.SwitchStmt:
		return m.parseSwitch(v)
	case *ast.ForStmt:
		return m.parseFor(v, "")
	case *ast.IncDecStmt:
		return m.parseIncDec(v)
	case *ast.BranchStmt:
		return m.parseBranch(v, f)
	case *ast.LabeledStmt:
		if f, o := v.Stmt.(*ast.ForStmt); o {
			return m.parseFor(f, v.Label.Name)
		} else {
			panic(position(v) + ": labeled statement must be a for loop")
		}
	case *ast.DeferStmt: // e.g. defer module.Catch(func)
		if c, o := m.parseExpr(v.Call).(Call); !o {
			panic(position(v) + ":defer expects call expression")
		} else {
			if c.Func != "Catch" {
				panic(position(v) + ":defer function must be 'Catch'")
			}
			m.current.Defer = Call{Func: string(c.Args[0].(GlobalGets)[0]), a: v}
		}
		return Nop{}
	default:
		panic(position(st) + ": unknown statement: " + reflectType(st))
	}
}
func (m *Module) parseAssign(a *ast.AssignStmt) (r Assign) {
	for i := range a.Lhs {
		s, g := m.varname(a.Lhs[i]), false
		r.Name = append(r.Name, s)
		if s != "_" {
			g = isglobal(a.Lhs[i])
		}
		r.Glob = append(r.Glob, g)
	}
	for i := range a.Rhs {
		r.Expr = append(r.Expr, m.parseExpr(a.Rhs[i]))
		_, t := parseTypes(a.Rhs[i])
		r.Typs = append(r.Typs, t...)
	}
	r.Mod = a.Tok.String()
	if r.Mod == ":=" {
		for i := range r.Name {
			m.current.Locs = append(m.current.Locs, Local{r.Name[i], r.Typs[i]})
		}
	}
	return r
}
func (m *Module) parseDecl(d *ast.GenDecl, globalscope int) Stmt {
	if d.Tok != token.VAR && d.Tok != token.CONST {
		panic(position(d) + ": expected variable|constant declaration")
	}
	if len(d.Specs) == 0 {
		return Nop{}
	}
	var r Stmts
	for _, s := range d.Specs {
		a := m.parseDeclSpec(s, globalscope, d.Tok)
		r = append(r, a)
	}
	return r
}
func (m *Module) parseDeclSpec(s ast.Spec, globalscope int, tok token.Token) (r Assign) {
	var names []string
	var glob []bool
	v := s.(*ast.ValueSpec)
	for i, n := range v.Names {
		var sn []string
		var st []Type
		if v.Type != nil { // var g int32
			sn, st = parseTypes(v.Type)
		} else if len(v.Values) > i { // var g = int32(1)
			sn = []string{""}
			st = []Type{parseType(info.TypeOf(v.Values[i]), position(v.Values[i]))}
		} else {
			sn = []string{""}
			st = []Type{parseType(info.TypeOf(n), position(n))}
		}
		for i := range sn {
			name := n.Name + m.lookup(scope(s))
			if sn[i] != "" {
				name = n.Name + "." + sn[i]
			}
			l := Local{name, st[i]}
			names = append(names, name)
			if globalscope == 0 {
				m.current.Locs = append(m.current.Locs, l)
			}
			glob = append(glob, isglobal(v))
		}
		r.Typs = append(r.Typs, st...)
	}
	r.Name = names
	r.Glob = glob
	r.Const = make([]bool, len(r.Name))
	if len(v.Values) > 0 {
		for i, e := range v.Values {
			if tok == token.CONST {
				t := info.Types[e]
				v := fmt.Sprint(t.Value)
				if l, ok := e.(*ast.BasicLit); ok {
					v = l.Value
				}
				r.Expr = append(r.Expr, Literal{Type: parseType(t.Type, position(e)), Value: v})
				r.Const[i] = true
			} else {
				r.Expr = append(r.Expr, m.parseExpr(e))
			}
		}
		r.Glob = glob
		return r // multiple specs?
	} else if globalscope > 0 {
		r.Name = names
		r.Expr = make([]Expr, len(r.Typs))
		for i, t := range r.Typs {
			r.Expr[i] = Literal{t, "0"}
		}
		return r
	}
	return r
}

func (m *Module) parseIncDec(a *ast.IncDecStmt) (r Assign) {
	s := m.varname(a.X)
	g := isglobal(a)
	r.Name = []string{s}
	r.Glob = []bool{g}
	if a.Tok == token.INC {
		r.Mod = "+="
	} else {
		r.Mod = "-="
	}
	t := parseType(info.TypeOf(a.X), position(a))
	r.Typs = []Type{t}
	r.Expr = []Expr{Literal{Type: t, Value: "1"}}
	return r
}
func (m *Module) parseBranch(a *ast.BranchStmt, f *For) (r Branch) {
	switch a.Tok {
	case token.BREAK, token.CONTINUE:
		r.Break = a.Tok == token.BREAK
		if f != nil && a.Tok == token.CONTINUE {
			f.Simple = true
		}
	default:
		panic(position(a) + ": unsupported branch statement")
	}
	if a.Label != nil {
		r.Label = a.Label.Name
	}
	return r
}
func (m *Module) parseIf(a *ast.IfStmt) (r Stmts) {
	var i If
	if a.Init != nil {
		r = append(r, m.parseStmt(a.Init, nil))
	}
	i.If = m.parseExpr(a.Cond)
	for _, st := range a.Body.List {
		i.Then = append(i.Then, m.parseStmt(st, nil))
	}
	if a.Else != nil {
		if b, o := a.Else.(*ast.BlockStmt); o {
			i.Else = m.parseStmts(b.List)
		} else {
			i.Else = Stmts{m.parseStmt(a.Else, nil)}
		}
	}
	return append(r, i)
}
func (m *Module) parseSwitch(a *ast.SwitchStmt) (r Switch) {
	if a.Init != nil {
		panic(position(a) + ": switch statement with init is not supported")
	}
	r.E = m.parseExpr(a.Tag)
	for i := range a.Body.List {
		c := a.Body.List[i].(*ast.CaseClause)
		if c.List == nil && i == len(a.Body.List)-1 {
			r.Def = m.parseStmts(c.Body)
		} else {
			if len(c.List) != 1 {
				panic(position(c) + ": multiple cases are not supported")
			} else {
				cs := info.Types[c.List[0]].Value.String()
				if cs != strconv.Itoa(i) {
					panic(position(c) + ": cases must be constants 0 1 2..")
				}
			}
			r.Case = append(r.Case, m.parseStmts(c.Body))
		}
	}
	return r
}
func (m *Module) parseFor(a *ast.ForStmt, label string) (r Stmts) {
	var f For
	if a.Init != nil {
		r = append(r, m.parseStmt(a.Init, nil))
	}
	var body Stmts
	for _, st := range a.Body.List {
		body = append(body, m.parseStmt(st, &f)) // may assign f.Simple
	}
	var cond Expr
	if a.Cond != nil {
		cond = m.parseExpr(a.Cond)
	}
	var post Stmt
	if a.Post != nil {
		post = m.parseStmt(a.Post, nil)
	}
	f.Cond = cond
	f.Post = post
	f.Body = body
	f.Label = label
	return append(r, f)
}
func catscopes(s *types.Scope) (r []*types.Scope) {
	r = append(r, s)
	for i := 0; i < s.NumChildren(); i++ {
		r = append(r, catscopes(s.Child(i))...)
	}
	return r
}
func scope(a ast.Node) *types.Scope {
	switch v := a.(type) {
	case *ast.Ident:
		return info.ObjectOf(v).Parent()
	case *ast.SelectorExpr:
		return scope(v.X)
	case *ast.ValueSpec:
		return scope(v.Names[0])
	case *ast.IncDecStmt:
		return scope(v.X)
	case *ast.GenDecl:
		return scope(v.Specs[0])
	default:
		panic(position(a) + ": unknown scope: " + reflect.TypeOf(v).String())
	}
}
func isglobal(a ast.Node) bool { return scope(a) == pkg.Scope() }
func (m *Module) lookup(s *types.Scope) string {
	for i := range m.scopes {
		if s == m.scopes[i] {
			if i == 0 {
				break
			}
			return strconv.Itoa(i)
		}
	}
	return ""
}
func (m *Module) varname(a ast.Node) string {
	switch v := a.(type) {
	case *ast.Ident:
		if v.Name == "_" {
			return "_"
		}
		return v.Name + m.lookup(scope(a))
	case *ast.SelectorExpr:
		return m.varname(v.X) + "." + v.Sel.Name
	default:
		panic(position(a) + ": unknown variable node: " + reflectType(a))
	}
}

//	func (m *Module) parseSimdMethod(a ast.Node, argv []ast.Expr) Expr {
//		if v, o := a.(*ast.SelectorExpr); o {
//			if vt, o := info.TypeOf(v.X).Underlying().(*types.Array); o {
//				st := parseSimdType(vt, position(a))
//				str := string(st)
//				recv := m.parseExpr(v.X)
//				args := []Expr{recv}
//				for i := range argv {
//					args = append(args, m.parseExpr(argv[i]))
//				}
//				return Call{
//					Func: fmt.Sprintf("%s.%s", str, v.Sel.Name),
//					Args: args,
//				}
//			}
//		}
//		return nil
//	}
func parseLiteral(a *ast.BasicLit, xt types.Type) (r Literal) {
	var t Type
	if b, o := info.TypeOf(a).(*types.Basic); o && (b.Kind() == types.UntypedInt || b.Kind() == types.Uint || b.Kind() == types.Int) && xt != nil {
		t = parseType(xt, position(a))
	} else {
		t = parseType(info.TypeOf(a).Underlying(), position(a))
	}
	r = Literal{
		Type:  t,
		Value: a.Value,
	}
	if strings.HasPrefix(r.Value, "'") {
		s := r.Value[1 : len(r.Value)-1]
		v, _, _, _ := strconv.UnquoteChar(s, 39)
		r.Value = strconv.Itoa(int(v))
	}
	return r
}
func (m *Module) parseReturn(a *ast.ReturnStmt) (r Return) {
	r = make(Return, len(a.Results))
	for i, x := range a.Results {
		r[i] = m.parseExpr(x)
	}
	return r
}
func (m *Module) parseCall(a *ast.CallExpr) Expr {
	if t, o := info.TypeOf(a.Fun).Underlying().(*types.Basic); o { //cast
		p := position(a)
		if len(a.Args) != 1 {
			panic(p + ": expected cast with 1 argument")
		}
		arg := a.Args[0]
		r := Cast{Dst: parseType(t, p), Src: parseType(info.TypeOf(arg).Underlying(), p), Arg: m.parseExpr(arg)}
		if r.Dst == F64 && (r.Src != I32 && r.Src != I64) {
			panic(p + ": unsupported cast: " + string(r.Src) + "->" + string(r.Dst))
		}
		if r.Src == F64 && (r.Dst != I32 && r.Dst != I64) {
			panic(p + ": unsupported cast: " + string(r.Dst) + "->" + string(r.Src))
		}
		if l, ok := r.Arg.(Literal); ok && l.Type == I32 && r.Dst == I64 {
			l.Type = I64
			r.Arg = l
		}
		return r
	}
	if ic, o := a.Fun.(*ast.TypeAssertExpr); o {
		return m.parseCallIndirect(ic, a.Args, a)
	}
	//	if e := m.parseSimdMethod(a.Fun, a.Args); e != nil {
	//		return e
	//	}
	name := m.varname(a.Fun)

	var args []Expr
	switch name {
	case "Memory": // module.Memory(1)
		l := a.Args[0].(*ast.BasicLit)
		m.Memory = l.Value
		return Nop{}
	case "Memory2": // module.Memory2(1)
		l := a.Args[0].(*ast.BasicLit)
		m.Memory2 = l.Value
		return Nop{}
	case "Functions": // module.Functions(0, f1, f2, ...)
		off, _ := strconv.Atoi(a.Args[0].(*ast.BasicLit).Value)
		var names []string
		for _, f := range a.Args[1:] {
			names = append(names, f.(*ast.Ident).Name)
		}
		m.Table = append(m.Table, TableEntries{off, names})
		return Nop{}
	case "Export": // module.Export(f1, f2...)
		for _, f := range a.Args {
			m.Exports[f.(*ast.Ident).Name] = true
		}
		return Nop{}
	case "ExportAll":
		m.exportAll = true
		return Nop{}
	case "Data": // module.Data(0, "...")
		off, _ := strconv.Atoi(a.Args[0].(*ast.BasicLit).Value)
		data, e := strconv.Unquote(a.Args[1].(*ast.BasicLit).Value)
		fatal(e)
		add := true
		for i := range m.Data { // some Data-calls are parsed twice. why?
			if m.Data[i].Off == off {
				add = false
			}
		}
		if add {
			min, max := off, off+len(data)
			for _, d := range m.Data {
				if max <= d.Off || min >= d.Off+len(d.Data) {
				} else {
					println(max, d.Off, min, d.Off+len(d.Data))
					panic(fmt.Sprintf("data section [%d .. %d] overlaps [%d .. %d]", min, max, d.Off, d.Off+len(d.Data)))
				}
			}
			if l := len(m.Data); l > 0 && off == m.Data[l-1].Off+len(m.Data[l-1].Data) {
				m.Data[l-1].Data += data
			} else {
				m.Data = append(m.Data, Data{off, data})
			}
		}
		return Nop{}
	case "Printf":
		return parsePrintf(a.Args)
	}
	if s, o := a.Fun.(*ast.SelectorExpr); o { //method receiver
		if id, o := s.X.(*ast.Ident); o {
			obj := info.ObjectOf(id)
			if p, o := obj.(*types.PkgName); o { //package call, e.g. wasi.Func(..)
				pkgname := p.Name()
				b := []byte(name)
				n := len(pkgname)
				b[1+n] += 32
				name = string(b)
				as, rs := m.parseSignature(a.Fun)
				m.Imports[name] = Import{pkgname, strings.TrimPrefix(name, pkgname+"."), as, rs}
				goto args
			}
		}
		name = strings.TrimPrefix(info.TypeOf(s.X).String(), "input.")
		name += "." + s.Sel.Name
		args = append(args, m.parseGets(s.X))
	}
args:
	for i := range a.Args {
		args = append(args, m.parseExpr(a.Args[i]))
	}
	return Call{Func: name, Args: args, a: a}
}
func (m *Module) parseCallIndirect(a *ast.TypeAssertExpr, args []ast.Expr, an ast.Node) (r CallIndirect) {
	ix, o := a.X.(*ast.IndexExpr)
	if o == false {
		panic(position(a) + ": type assertion: expected indirect function call")
	}
	if f, o := ix.X.(*ast.Ident); !o || f.Name != "Func" {
		panic(position(a) + ": indirect function call must index into \"Func\"")
	}
	r.Func = m.parseExpr(ix.Index)
	r.ArgType, r.ResType = m.parseSignature(a.Type)
	r.Args = make([]Expr, len(args))
	r.a = an
	for i := range args {
		r.Args[i] = m.parseExpr(args[i])
	}
	return r
}
func parsePrintf(a []ast.Expr) (p Printf) {
	p.Format = a[0].(*ast.BasicLit).Value
	for _, v := range a[1:] {
		p.Args = append(p.Args, v.(*ast.Ident).Name)
	}
	return p
}
func parseIdent(a ast.Node) string { // x or x.a or x.a.b
	switch v := a.(type) {
	case *ast.Ident:
		return v.Name
	case *ast.SelectorExpr:
		return parseIdent(v.X) + "." + v.Sel.Name
	default:
		panic(position(a) + ": unknown type: " + reflectType(a))
	}
}
func (m *Module) parseIdents(a ast.Node) (r []string) { // x(maybe struct) x.a.b(maybe embedded)
	switch v := a.(type) {
	case *ast.Ident:
		s := m.lookup(scope(v))
		if info.TypeOf(v) == nil {
			return []string{v.Name + s}
		}
		return m.structVars(v.Name, info.TypeOf(v).Underlying(), s)
	case *ast.SelectorExpr:
		s := m.lookup(scope(v))
		t := info.TypeOf(v).Underlying()
		p := parseIdent(v.X) + "." + v.Sel.Name
		r = m.structVars(p, t, s)
		return r
	default:
		panic(position(a) + ": unknown type: " + reflectType(a))
	}
}
func (m *Module) structVars(s string, a types.Type, scope string) (names []string) {
	if st, o := a.(*types.Struct); o {
		for i := 0; i < st.NumFields(); i++ {
			f := st.Field(i)
			n := m.structVars(s+"."+f.Name(), f.Type().Underlying(), scope)
			names = append(names, n...)
		}
		return names
	} else {
		return []string{s + scope}
	}
}
func (m *Module) parseGets(a ast.Node) Expr {
	if i, o := a.(*ast.Ident); o {
		if i.Name == "false" {
			return Literal{I32, "0"}
		} else if i.Name == "true" {
			return Literal{I32, "1"}
		}
	}
	if isglobal(a) {
		return GlobalGets(m.parseIdents(a))
	} else {
		return LocalGets(m.parseIdents(a))
	}
}
func (m *Module) parseExpr(a ast.Expr) Expr {
	switch v := a.(type) {
	case *ast.UnaryExpr:
		x := m.parseExpr(v.X)
		if l, ok := x.(Literal); ok && v.Op.String() == "-" {
			l.Value = "-" + l.Value
			return l
		}
		return Unary{
			X:  x,
			Op: Op{Name: v.Op.String(), Type: parseType(info.TypeOf(v.X), position(v))},
		}
	case *ast.BinaryExpr:
		var y Expr
		if l, o := v.Y.(*ast.BasicLit); o {
			y = parseLiteral(l, info.TypeOf(v.X))
		} else {
			y = m.parseExpr(v.Y)
		}
		return Binary{
			X:  m.parseExpr(v.X),
			Y:  y,
			Op: Op{Name: v.Op.String(), Type: parseType(info.TypeOf(v.X), position(v))},
		}
	case *ast.Ident, *ast.SelectorExpr:
		return m.parseGets(a)
	case *ast.BasicLit:
		return parseLiteral(v, nil)
	case *ast.CallExpr:
		return m.parseCall(v)
	case *ast.ParenExpr:
		return m.parseExpr(v.X)
	default:
		panic(position(a) + ": unknown expr: " + reflectType(a))
	}
}
