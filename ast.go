package wg

import (
	"go/ast"
	"go/types"
	"io"
	"strings"
)

type Type string
type Emitter interface {
	wat(io.Writer)
	c(io.Writer)
}

const (
	V     Type = ""
	I32        = "i32"
	U32        = "u32"
	I64        = "i64"
	U64        = "u64"
	F32        = "f32"
	F64        = "f64"
	I8x16      = "I8x16"
	I32x4      = "I32x4"
	F64x2      = "F64x2"
)

func (t Type) String() string {
	r := string(t)
	if strings.HasPrefix(r, "u") {
		return "i" + r[1:]
	}
	if strings.Index(r, "x") > 0 {
		return "v128"
	}
	return r
}

type Module struct {
	Package   string
	Memory    string
	Memory2   string
	Imports   map[string]Import
	Exports   map[string]bool
	Globals   []Assign
	Funcs     []Func
	Table     []TableEntries
	Data      []Data
	current   *Func
	scopes    []*types.Scope
	exportAll bool
}
type TableEntries struct {
	Off   int
	Names []string
}
type Import struct {
	Package string
	Func    string
	Arg     []Type
	Res     []Type
}
type Data struct {
	Off  int
	Data string
}
type Func struct {
	Name     string
	Args     []Arg
	Rets     []Type
	Locs     []Local // without args or rets
	Body     Stmts
	Doc      string
	Defer    Expr // Call
	Exported bool
}
type Arg struct {
	Name string
	Type Type
}
type Local struct {
	Name string
	Type Type
}
type Stmt Emitter
type Stmts []Stmt
type Assign struct { //Stmt
	Name  []string
	Expr  []Expr
	Glob  []bool
	Const []bool
	Typs  []Type
	Mod   string
}
type Return []Expr //Stmt
type Drop struct {
	Expr
}
type Nop struct{}
type Expr Emitter
type Unary struct { //Expr
	X  Expr
	Op Op
}
type Binary struct { //Expr
	X, Y Expr
	Op   Op
}
type Literal struct {
	Type  Type
	Value string
}
type GlobalGet string    //Expr
type GlobalGets []string //Expr
type LocalGet string     //Expr
type LocalGets []string  //Expr (struct)
type Op struct {
	Name string
	Type Type
}
type Call struct { //Expr
	Func string
	Args []Expr
	a    ast.Node
}
type Cast struct {
	Dst, Src Type
	Arg      Expr
}
type CallIndirect struct {
	Func    Expr
	Args    []Expr
	ArgType []Type
	ResType []Type
	a       ast.Node
}
type If struct {
	If         Expr
	Then, Else Stmts
}
type Switch struct {
	E    Expr
	Case []Stmts
	Def  Stmts
}
type For struct {
	Cond   Expr
	Post   Stmt
	Body   Stmts
	Label  string
	Simple bool
}
type Branch struct {
	Break bool // break|continue
	Label string
}
type Printf struct {
	Format string
	Args   []string
}
