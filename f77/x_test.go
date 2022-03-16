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

type K uint64
type T uint32

func main() {
	//SetI32(8, 0xff0011)
	//x := negate(float64(Add(1, 2)))
	//x := negate(Add(1, 2))
	a := int32(3)
	x := K(uint64(a)<<uint64(59)) | 456
	Printf("x,a", x, a)

	t := tp(x)
	//x := int32(3) + int32(4)
	Printf("x,t", x, t)

}
func Add(x, y int32) int32 { return x + y }

func negate(x float64) float64 { return -x }
func tp(x K) T                 { return T(x >> 59) }

//func negate(x int32) int32 { return -x }
