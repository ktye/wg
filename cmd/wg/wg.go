package main

import (
	"fmt"
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

func main() { wg(os.Args[1:]) }
func wg(args []string) {
	if len(args) != 1 {
		panic("args")
	}

	var f *ast.File
	var e error
	fset = token.NewFileSet()
	path := args[0]
	if strings.HasSuffix(path, ".go") {
		f, e = parser.ParseFile(fset, path, nil, 0)
		fatal(e)
	} else {
		notest := func(f fs.FileInfo) bool { return !strings.HasSuffix(f.Name(), "_test.go") }
		pkgs, e := parser.ParseDir(fset, path, notest, 0)
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

	ast.Print(fset, f)
	parse(f)
}
func fatal(e error) {
	if e != nil {
		panic(e)
	}
}
func position(a ast.Node) string { return fset.Position(a.Pos()).String() }
func parse(a *ast.File) {
	f := funcs(a)
	fmt.Printf("%+v\n", f)
}
func funcs(a *ast.File) (r []Func) {
	for _, d := range a.Decls {
		if f, o := d.(*ast.FuncDecl); o {
			r = append(r, parseFunc(f))
		}
	}
	return r
}
func parseFunc(a *ast.FuncDecl) (f Func) {
	f.Name = a.Name.Name
	f.Args = parseArgs(a.Type.Params.List)
	r := parseArgs(a.Type.Results.List)
	fmt.Println("R", r)
	f.Rets = make([]Type, len(r))
	for i := range r {
		f.Rets[i] = r[i].Type
	}
	f.Body = parseBody(a.Body.List)

	//
	f.wat(os.Stdout)

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
				name += "_" + s
			}
			r = append(r, Arg{name, Type(t[j])})
		}
	}
	return r
}
func parseTypes(a ast.Expr) (names []string, rtype []Type) {
	pos := position(a)
	u := info.TypeOf(a).Underlying()
	switch v := u.(type) {
	case *types.Basic:
		return []string{""}, []Type{parseType(u, pos)}
	case *types.Struct:
		for i := 0; i < v.NumFields(); i++ {
			x := v.Field(i)
			names = append(names, x.Name())
			rtype = append(rtype, parseType(x.Type(), pos))
		}
		return names, rtype
	default:
		panic(position(a) + ": unknown type")
	}
}
func parseType(t types.Type, pos string) Type {
	s := t.Underlying().String()
	switch s {
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
		t := reflect.TypeOf(st).String()
		panic(position(st) + ": unknown statement: " + t)
	}
}
func parseAssign(a *ast.AssignStmt) (r Assign) {
	for i := range a.Lhs {
		l, o := a.Lhs[i].(*ast.Ident)
		if o == false {
			panic(position(a) + ": lhs is not an identifier")
		}
		r.Name = append(r.Name, l.Name)
	}
	for i := range a.Rhs {
		r.Expr = append(r.Expr, parseExpr(a.Rhs[i]))
	}
	return r
}
func parseReturn(a *ast.ReturnStmt) (r Return) {
	r = make(Return, len(a.Results))
	for i, x := range a.Results {
		r[i] = parseExpr(x)
	}
	return r
}
func parseExpr(a ast.Expr) Expr {
	switch v := a.(type) {
	case *ast.BinaryExpr:
		return Binary{
			X:  parseExpr(v.X),
			Y:  parseExpr(v.Y),
			Op: Op{Name: v.Op.String(), Type: parseType(info.TypeOf(v.X), position(v))},
		}
	case *ast.Ident:
		return LocalGet(v.Name)
	default:
		panic(position(a) + ": unknown expr: " + reflect.TypeOf(a).String())
	}
}
