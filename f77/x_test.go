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

type K uint64
type T uint32
type f1 = func(int32) int32
type f2 = func(int32, int32) int32

func main() {
	a, b := float64(3), float64(4)
	r := hypot(a, b)
	Printf("r", r)
}
func hypot(x, y float64) float64 { return 0 }