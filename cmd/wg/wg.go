package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"io/fs"
	"os"
	"reflect"
	"strings"
)

var fset *token.FileSet
var info types.Info
var printast bool

func main() {
	m := parse(os.Args[1])
	m.wat(os.Stdout)
}
func parse(path string) Module { // file.go or dir
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
	info = types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
		//Defs:  make(map[*ast.Ident]types.Object),
		//Uses:  make(map[*ast.Ident]types.Object),
	}
	_, e = conf.Check("input", fset, []*ast.File{f}, &info)
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
	r.Funcs = parseFuncs(a)
	return r
}
func parseFuncs(a *ast.File) (r []Func) {
	for _, d := range a.Decls {
		if f, o := d.(*ast.FuncDecl); o {
			r = append(r, parseFunc(f))
		}
	}
	return r
}
func parseFunc(a *ast.FuncDecl) (f Func) {
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
		}
	}
	f.Body = parseBody(a.Body.List)
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
func parseType(t types.Type, pos string) Type {
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
func parseBody(st []ast.Stmt) (r []Stmt) {
	for i := range st {
		r = append(r, parseStmt(st[i]))
	}
	return r
}
func parseStmt(st ast.Stmt) Stmt {
	switch v := st.(type) {
	case *ast.AssignStmt:
		return parseAssign(v)
	case *ast.ReturnStmt:
		return parseReturn(v)
	default:
		panic(position(st) + ": unknown statement: " + reflectType(st))
	}
}
func parseAssign(a *ast.AssignStmt) (r Assign) {
	for i := range a.Lhs {
		r.Name = append(r.Name, varname(a.Lhs[i]))
	}
	for i := range a.Rhs {
		r.Expr = append(r.Expr, parseExpr(a.Rhs[i]))
	}
	r.Type = parseType(info.TypeOf(a.Rhs[0]), position(a))
	r.Mod = a.Tok.String()
	return r
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
func parseReturn(a *ast.ReturnStmt) (r Return) {
	r = make(Return, len(a.Results))
	for i, x := range a.Results {
		r[i] = parseExpr(x)
	}
	return r
}
func parseCall(a *ast.CallExpr) Expr {
	if id, o := a.Fun.(*ast.Ident); o && (id.Obj == nil || id.Obj.Kind == ast.Typ) {
		arg := a.Args[0]
		p := position(a)
		ltype := parseType(info.TypeOf(a), p)
		rtype := parseType(info.TypeOf(a), p)
		if ltype == rtype {
			return LocalGet(varname(arg))
		} else {
			panic("cast..")
		}
	}
	var args []Expr
	name := varname(a.Fun)
	if s, o := a.Fun.(*ast.SelectorExpr); o { //method receiver
		name = strings.TrimPrefix(info.TypeOf(s.X).String(), "input.")
		name += "." + s.Sel.Name
		args = append(args, parseGets(s.X))
	}
	for i := range a.Args {
		args = append(args, parseExpr(a.Args[i]))
	}
	return Call{Func: name, Args: args}
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
			names = append(names, s+"."+f.Name())
		}
		return names
	} else {
		return []string{s}
	}
}
func parseGets(a ast.Node) Expr { return LocalGets(parseIdents(a)) }
func parseExpr(a ast.Expr) Expr {
	switch v := a.(type) {
	case *ast.UnaryExpr:
		return Unary{
			X:  parseExpr(v.X),
			Op: Op{Name: v.Op.String(), Type: parseType(info.TypeOf(v.X), position(v))},
		}
	case *ast.BinaryExpr:
		return Binary{
			X:  parseExpr(v.X),
			Y:  parseExpr(v.Y),
			Op: Op{Name: v.Op.String(), Type: parseType(info.TypeOf(v.X), position(v))},
		}
	case *ast.Ident, *ast.SelectorExpr:
		return parseGets(a)
	case *ast.BasicLit:
		return parseLiteral(v)
	case *ast.CallExpr:
		return parseCall(v)
	default:
		panic(position(a) + ": unknown expr: " + reflectType(a))
	}
}
