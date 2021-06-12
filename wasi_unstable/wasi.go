package wasi_unstable

import (
	"fmt"
	"io/ioutil"
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
	addr := module.I32(p)
	n := module.I32(4 + p)
	if fd == 0 {
		nr, err := os.Stdin.Read(module.Bytes[addr : addr+n])
		module.SetI32(written, int32(nr))
		if err == nil {
			return 0
		}
		return 1
	}
	b, o := files[fd]
	if o == false {
		return 1
	}
	copy(module.Bytes[addr:], b[:n])
	files[fd] = b[n:]
	module.SetI32(written, n)
	return 0
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
func Fd_seek(fp int32, offset int64, whence int32, rp int32) int32 {
	b, o := files[fp]
	if o == false {
		return 1
	}
	if whence == 2 {
		module.SetI32(rp, int32(len(b)))
	} else {
		module.SetI32(rp, 0)
	}
	return 0
}

var files map[int32][]byte

func Path_open(fd, dirflags, path, pathlen, oflags int32, baserights, inheritrights int64, fdflags, newfp int32) int32 {
	b, e := ioutil.ReadFile(string(module.Bytes[path : path+pathlen]))
	if e != nil {
		fmt.Println(e)
		return 1
	}
	fp := int32(2 + len(files))
	files[fp] = b
	module.SetI32(newfp, fp)
	return 0
}

func Fd_close(x int32) int32 { delete(files, x); return 0 }
func Proc_exit(x int32)      { os.Exit(int(x)) }

func Args_sizes_get(np, sp int32) int32 {
	s, n := getargs()
	module.SetI32(np, n)
	module.SetI32(sp, int32(len(s)))
	return 0
}
func Args_get(p, sp int32) int32 {
	s, _ := getargs()
	// p (**argv) is ignored
	copy(module.Bytes[sp:], s)
	return 0
}
func getargs() (r []byte, n int32) {
	for _, a := range os.Args {
		r = append(r, []byte(a)...)
		r = append(r, 0)
		n++
	}
	return r, n
}

func init() {
	files = make(map[int32][]byte)
}
