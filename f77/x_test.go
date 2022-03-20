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
	for i := int32(0); i < 300; i++ {
		SetI8(i, 0)
	}
	SetI8(5, 0)
	SetI8(7, 0)
	x := fwh(0, 10)
	Printf("x", x)
}
func fwh(a, b int32) int32 { return -1 }
