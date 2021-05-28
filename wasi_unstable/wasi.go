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

func Clock_time_get(id ClockID, prec Timestamp) Timestamp { return Timestamp(time.Now().UnixNano()) }

// Memory at p:
//   p[0:4]: addr, e.g. 8
//   p[4:8]: number of bytes to write
//   p[8:]:  data
// written is a memory location that receive the number of bytes written.
func Fd_write(fd, p, niovec, written int32) int32 {
	if fd == 1 {
		addr := module.I32(p) // or p+addr?
		n := module.I32(4 + p)
		nw, _ := os.Stdout.Write(module.Bytes[addr : addr+n])
		module.SetI32(written, int32(nw))
	}
	return 0
}
