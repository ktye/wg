//go:build ignore
// +build ignore

package f

import . "github.com/ktye/wg/module"

// Init sets up the module. The function is not compiled to wasm.
func init() {
	Memory(1)
	//Functions(0, Add)
	//Data(0, "abc")
}

func main() {
	//SetI32(8, 0xff0011)
	x := negate(float64(Add(1, 2)))
	//x := negate(Add(1, 2))
	Printf("x", x)
}
func Add(x, y int32) int32 { return x + y }

func negate(x float64) float64 { return -x }

//func negate(x int32) int32 { return -x }
