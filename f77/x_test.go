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
	x := int32(1)
	y := int32(2)
	if x > 0 && y > 0 {
		Printf("x", x)
	}
}
func f() (r int64) {
	r = int64(1)
	return r
}
