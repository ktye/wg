package main

import "io"

type Type string

const (
	V   Type = ""
	I32      = "i32"
	U32      = "u32"
	I64      = "i64"
	U64      = "u64"
	F32      = "f32"
	F64      = "f64"
)

type Module struct {
	Funcs []Func
}
type Func struct {
	Name string
	Args []Arg
	Rets []Type
	Body []Stmt
	Doc  string
}
type Arg struct {
	Name string
	Type Type
}
type Stmt Emitter
type Assign struct { //Stmt
	Name []string
	Expr []Expr
	Type Type
	Mod  string
}
type Return []Expr //Stmt
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
type LocalGet string    //Expr
type LocalGets []string //Expr (struct)
type Op struct {
	Name string
	Type Type
}

type Emitter interface {
	wat(io.Writer)
}
