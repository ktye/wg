//go:build ignore
// +build ignore

package f

import (
	. "github.com/ktye/wg/module"
)

// Init sets up the module. The function is not compiled to wasm.
func init() {
	Memory(1)
	//Functions(0, Add, Sub, Neg)
	//Data(0, "abc")
}

const E int32 = 1

func main() {
	var r, q int32
	q = 1
	r = 1
	if match(r, q) == 0 {
		dx(E)
	}
	dx(q)
}
func match(x, y int32) int32 { return 1 }
func dx(x int32) int32       { return x }
func trp(x int32)            {}
