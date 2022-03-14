//go:build ignore
// +build ignore

package f

//	. "github.com/ktye/wg/module"

// Init sets up the module. The function is not compiled to wasm.
func init() {
	// Memory(1)
	//Functions(0, Add)
	//Data(0, "abc")
}

func Add(x, y int32) int32 { return x + y }
