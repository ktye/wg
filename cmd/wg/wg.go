package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"strings"
)

var fset *token.FileSet

func main() {
	args := os.Args[1:]
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
	ast.Print(fset, f)
	wat(f)
}
func fatal(e error) {
	if e != nil {
		panic(e)
	}
}
func wat(f *ast.File) {
	ast.Inspect(f, inspectFuncs)
}
func inspectFuncs(n ast.Node) bool {
	f, o := n.(*ast.FuncDecl)
	if !o {
		return true
	}
	fmt.Println(f.Name.Name)
	return true
}
