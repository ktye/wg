package wasi_unstable

import (
	"os"
	"time"

	"github.com/ktye/wg/module"
)

type ClockID int32
type Timestamp uint64

const (
	Realtime ClockID = iota
	Monotonic
	ProcessCPU
	ThreadCPU
)

//func L32(x int32) int32   { fmt.Println(x); return x }
//func L64(x uint64) uint64 { fmt.Println(x); return x }
func Clock_time_get(id ClockID, prec Timestamp, res int32) int32 {
	module.SetI64(res, time.Now().UnixNano())
	return 0
}

// Memory at p:
//   p[0:4]: addr, e.g. 8
//   p[4:8]: number of bytes to write
//   p[8:]:  data
// written is a memory location that receive the number of bytes written.
func Fd_read(fd, p, niovec, written int32) int32 {
	if fd == 0 {
		addr := module.I32(p)
		n := module.I32(4 + p)
		nr, err := os.Stdin.Read(module.Bytes[addr : addr+n])
		module.SetI32(written, int32(nr))
		if err == nil {
			return 0
		}
	}
	return 1
}
func Fd_write(fd, p, niovec, written int32) int32 {
	if fd == 1 {
		addr := module.I32(p)
		n := module.I32(4 + p)
		nw, err := os.Stdout.Write(module.Bytes[addr : addr+n])
		module.SetI32(written, int32(nw))
		if err == nil {
			return 0
		}
	}
	return 1
}
func Proc_exit(x int32) { os.Exit(int(x)) }
