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

func main() {
	var x int32
	x, _ = e(1)
	Printf("%12d%12d\n", x)
}
func e(x int32) (int32, int32) {
	return x + 1, x + 2
}
