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

type Func struct {
	Name string
	Args []Arg
	Rets []Type
	Body []Stmt
}
type Arg struct {
	Name string
	Type Type
}
type Stmt Emitter
type Assign struct { //Stmt
	Name []string
	Expr []Expr
}
type Return []Expr //Stmt
type Expr Emitter
type Binary struct { //Expr
	X, Y Expr
	Op   Op
}
type LocalGet string //Expr
type Op struct {
	Name string
	Type Type
}

type Emitter interface {
	wat(io.Writer)
}
