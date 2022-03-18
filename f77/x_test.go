//go:build ignore
// +build ignore

package f

import . "github.com/ktye/wg/module"

// Init sets up the module. The function is not compiled to wasm.
func init() {
	Memory(1)
	Functions(0, Add, Sub, Neg)
	//Data(0, "abc")
}

type K uint64
type T uint32
type f1 = func(int32) int32
type f2 = func(int32, int32) int32

func main() {
	x := int32(1)
	y := int32(2)
	c := int32(0)
	r := Func[c].(f2)(x, y)
	Printf("r", r)
}
func Add(x, y int32) int32 {
	x = -999
	return x + y
}
func Sub(x, y int32) int32 { return x - y }
func Neg(x int32) int32    { return -x }
