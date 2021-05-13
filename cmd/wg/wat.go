package main

import (
	"fmt"
	"io"
)

// convert wg ast to webassembly text format

func (f Func) wat(w io.Writer) {
	fmt.Fprintf(w, "(func $%s", f.Name)
	for _, a := range f.Args {
		fmt.Fprintf(w, " (param $%s %s)", a.Name, a.Type)
	}
	for _, a := range f.Rets {
		fmt.Fprintf(w, " (result %s)", a)
	}
	fmt.Fprintf(w, "\n")
	for _, st := range f.Body {
		st.wat(w)
	}
	fmt.Fprintf(w, ")\n")
}

func (a Assign) wat(w io.Writer) {
	for _, e := range a.Expr {
		e.wat(w)
	}
	for _, n := range a.Name {
		fmt.Fprintf(w, "local.set $%s\n", n)
	}
}
func (r Return) wat(w io.Writer) {
	for _, e := range r {
		e.wat(w)
	}
}
func (l LocalGet) wat(w io.Writer) { fmt.Printf("local.get $%s\n", l) }
func (b Binary) wat(w io.Writer) {
	s := string(b.Op.Type) + b.Op.Name
	op, ok := wasmops[s]
	if ok == false {
		panic("unknown wasm op: " + s)
	}
	b.X.wat(w)
	b.Y.wat(w)
	fmt.Fprintln(w, op)
}

var wasmops = map[string]string{
	"i32+": "int32.add",
}
