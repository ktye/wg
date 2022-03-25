//go:build ignore
// +build ignore

package f

import (
	. "github.com/ktye/wg/module"
)

// Init sets up the module. The function is not compiled to wasm.
func init() {
	Memory(1)
}

func main() {
	var i, m int32
	n := Args()
	//Printf("args i=%12d\n", n)
	for i = int32(0); i < n; i++ {
		//Printf("arg%12d\n", i)
		m = Arg(i, 0)
		Arg(i, 10)
		SetI8(10+m, 10)
		Write(0, 0, 10, 1+m)
	}
}
