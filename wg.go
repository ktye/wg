package wg

import (
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
			f = ast.MergePackageFiles(p, 0)
		}
	}

	var conf types.Config
	conf.Importer = importer.For("source", nil) // importer.Default()
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
func position(a ast.Node) string       { return fset.Position(a.Pos()).String() }
func reflectType(a interface{}) string { return reflect.TypeOf(a).String() }
func parseFile(a *ast.File) (r Module) {
	r.Globals = r.parseGlobals(a)
	r.Funcs = r.parseFuncs(a)
	return r
}
func (m *Module) parseGlobals(a *ast.File) (r []Assign) {
	for _, d := range a.Decls {
		if g, o := d.(*ast.GenDecl); o && g.Tok == token.VAR {
			r = append(r, m.parseDecl(g, true).(Assign))
		}
	}
	return r
}
func (m *Module) parseFuncs(a *ast.File) (r []Func) {
	for _, d := range a.Decls {
		if f, o := d.(*ast.FuncDecl); o {
			ff := m.parseFunc(f)
			if ff.Name != "init" {
				r = append(r, m.parseFunc(f))
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
		default:
			panic(position(a) + ": unknown type")
		}
	}
	return f("", info.TypeOf(a).Underlying())
}
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
	case "bool":
		return U32
	case "int32":
		return I32
	case "uint32":
		return U32
	case "int64":
		return I64
	case "uint64":
		return U64
	case "float32":
		return F32
	case "float64":
		return F64
	default:
		panic(pos + ": unknown type: " + s)
	}
}
func (m *Module) parseBody(st []ast.Stmt) (r []Stmt) {
	for i := range st {
		r = append(r, m.parseStmt(st[i]))
	}
	return r
}
func (m *Module) parseStmt(st ast.Stmt) Stmt {
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
		return m.parseDecl(v.Decl.(*ast.GenDecl), false)
	case *ast.IfStmt:
		return m.parseIf(v)
	case *ast.ForStmt:
		return m.parseFor(v, "")
	case *ast.IncDecStmt:
		return m.parseIncDec(v)
	case *ast.BranchStmt:
		return m.parseBranch(v)
	case *ast.LabeledStmt:
		if f, o := v.Stmt.(*ast.ForStmt); o {
			return m.parseFor(f, v.Label.Name)
		} else {
			panic(position(v) + ": labeled statement must be a for loop")
		}
	default:
		panic(position(st) + ": unknown statement: " + reflectType(st))
	}
}
func (m *Module) parseAssign(a *ast.AssignStmt) (r Assign) {
	for i := range a.Lhs {
		r.Name = append(r.Name, varname(a.Lhs[i]))
		r.Glob = append(r.Glob, isglobal(a.Lhs[i]))
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
func (m *Module) parseDecl(d *ast.GenDecl, globalscope bool) Stmt {
	var r Assign
	if d.Tok != token.VAR {
		panic(position(d) + ": expected variable declaration")
	}
	if len(d.Specs) > 1 {
		panic(position(d) + ": multiple specs?")
	}
	for _, s := range d.Specs {
		var names []string
		var glob []bool
		v := s.(*ast.ValueSpec)
		for i, n := range v.Names {
			var sn []string
			var st []Type
			if v.Type != nil { // var g int32
				sn, st = parseTypes(v.Type)
			} else { // var g = int32(1)
				sn = []string{""}
				st = []Type{parseType(info.TypeOf(v.Values[i]), position(v.Values[i]))}
			}
			for i := range sn {
				name := n.Name
				if sn[i] != "" {
					name = n.Name + "." + sn[i]
				}
				l := Local{name, st[i]}
				names = append(names, name)
				if globalscope == false {
					m.current.Locs = append(m.current.Locs, l)
				}
			}
			g := isglobal(v)
			for range names {
				glob = append(glob, g)
			}
			r.Typs = append(r.Typs, st...)
		}
		r.Name = names
		if len(v.Values) > 0 {
			for _, e := range v.Values {
				r.Expr = append(r.Expr, m.parseExpr(e))
			}
			r.Glob = glob
			return r // multiple specs?
		} else if globalscope {
			r.Name = names
			r.Expr = make([]Expr, len(r.Typs))
			for i, t := range r.Typs {
				r.Expr[i] = Literal{t, "0"}
			}
			return r
		}
	}
	return Nop{}
}
func (m *Module) parseIncDec(a *ast.IncDecStmt) (r Assign) {
	s := varname(a.X)
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
func (m *Module) parseBranch(a *ast.BranchStmt) (r Branch) {
	switch a.Tok {
	case token.BREAK, token.CONTINUE:
		r.Break = a.Tok == token.BREAK
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
		r = append(r, m.parseStmt(a.Init))
	}
	i.If = m.parseExpr(a.Cond)
	for _, st := range a.Body.List {
		i.Then = append(i.Then, m.parseStmt(st))
	}
	if a.Else != nil {
		b := a.Else.(*ast.BlockStmt)
		for _, st := range b.List {
			i.Else = append(i.Else, m.parseStmt(st))
		}
	}
	return append(r, i)
}
func (m *Module) parseFor(a *ast.ForStmt, label string) (r Stmts) {
	if a.Init != nil {
		r = append(r, m.parseStmt(a.Init))
	}
	var body Stmts
	for _, st := range a.Body.List {
		body = append(body, m.parseStmt(st))
	}
	var cond Expr
	if a.Cond != nil {
		cond = m.parseExpr(a.Cond)
	}
	var post Stmt
	if a.Post != nil {
		post = m.parseStmt(a.Post)
	}
	return append(r, For{Cond: cond, Post: post, Body: body, Label: label})
}
func isglobal(a ast.Node) bool {
	switch v := a.(type) {
	case *ast.Ident:
		return info.ObjectOf(v).Parent() == pkg.Scope()
	case *ast.SelectorExpr:
		return isglobal(v.X)
	}
	return false
}
func varname(a ast.Node) string {
	switch v := a.(type) {
	case *ast.Ident:
		return v.Name
	case *ast.SelectorExpr:
		return varname(v.X) + "." + v.Sel.Name
	default:
		panic(position(a) + ": unknown variable node: " + reflectType(a))
	}
}
func parseLiteral(a *ast.BasicLit) (r Literal) {
	return Literal{
		Type:  parseType(info.TypeOf(a), position(a)),
		Value: a.Value,
	}
}
func (m *Module) parseReturn(a *ast.ReturnStmt) (r Return) {
	r.List = make([]Expr, len(a.Results))
	for i, x := range a.Results {
		r.List[i] = m.parseExpr(x)
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
		return Cast{Dst: parseType(t, p), Src: parseType(info.TypeOf(arg).Underlying(), p), Arg: m.parseExpr(arg)}
	}
	if ic, o := a.Fun.(*ast.TypeAssertExpr); o {
		return m.parseCallIndirect(ic, a.Args)
	}
	name := varname(a.Fun)

	var args []Expr
	switch name {
	case "Memory": // module.Memory(1)
		l := a.Args[0].(*ast.BasicLit)
		m.Memory = l.Value
		return Nop{}
	case "Functions": // module.Functions(0, f1, f2, ...)
		off, _ := strconv.Atoi(a.Args[0].(*ast.BasicLit).Value)
		var names []string
		for _, f := range a.Args[1:] {
			names = append(names, f.(*ast.Ident).Name)
		}
		m.Table = append(m.Table, TableEntries{off, names})
		return Nop{}
	}
	if s, o := a.Fun.(*ast.SelectorExpr); o { //method receiver
		name = strings.TrimPrefix(info.TypeOf(s.X).String(), "input.")
		name += "." + s.Sel.Name
		args = append(args, parseGets(s.X))
	}
	for i := range a.Args {
		args = append(args, m.parseExpr(a.Args[i]))
	}
	return Call{Func: name, Args: args}
}
func (m *Module) parseCallIndirect(a *ast.TypeAssertExpr, args []ast.Expr) (r CallIndirect) {
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
	for i := range args {
		r.Args[i] = m.parseExpr(args[i])
	}
	return r
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
func parseIdents(a ast.Node) (r []string) { // x(maybe struct) x.a.b(maybe embedded)
	switch v := a.(type) {
	case *ast.Ident:
		if info.TypeOf(v) == nil {
			return []string{v.Name}
		}
		return structVars(v.Name, info.TypeOf(v).Underlying())
	case *ast.SelectorExpr:
		t := info.TypeOf(v).Underlying()
		p := parseIdent(v.X) + "." + v.Sel.Name
		r = structVars(p, t)
		return r
	default:
		panic(position(a) + ": unknown type: " + reflectType(a))
	}
}
func structVars(s string, a types.Type) (names []string) {
	if st, o := a.(*types.Struct); o {
		for i := 0; i < st.NumFields(); i++ {
			f := st.Field(i)
			n := structVars(s+"."+f.Name(), f.Type().Underlying())
			names = append(names, n...)
		}
		return names
	} else {
		return []string{s}
	}
}
func parseGets(a ast.Node) Expr {
	if isglobal(a) {
		return GlobalGets(parseIdents(a))
	} else {
		return LocalGets(parseIdents(a))
	}
}
func (m *Module) parseExpr(a ast.Expr) Expr {
	switch v := a.(type) {
	case *ast.UnaryExpr:
		return Unary{
			X:  m.parseExpr(v.X),
			Op: Op{Name: v.Op.String(), Type: parseType(info.TypeOf(v.X), position(v))},
		}
	case *ast.BinaryExpr:
		return Binary{
			X:  m.parseExpr(v.X),
			Y:  m.parseExpr(v.Y),
			Op: Op{Name: v.Op.String(), Type: parseType(info.TypeOf(v.X), position(v))},
		}
	case *ast.Ident, *ast.SelectorExpr:
		return parseGets(a)
	case *ast.BasicLit:
		return parseLiteral(v)
	case *ast.CallExpr:
		return m.parseCall(v)
	default:
		panic(position(a) + ": unknown expr: " + reflectType(a))
	}
}
