package wasi_example

import (
	. "github.com/ktye/wg/module"
	"github.com/ktye/wg/wasi_unstable"
)

func init() {
	Memory(1)
	Export(_start)
}

func _start() {
	SetI32(0, 8)
	SetI32(4, 4)
	SetI8(8, 'a')
	SetI8(9, 'b')
	SetI8(10, 'c')
	SetI8(11, 10)
	wasi_unstable.Fd_write(1, 0, 1, 12)
}
