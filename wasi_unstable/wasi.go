package wasi_unstable

import (
	"fmt"
	"io"
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

var (
	Stdout io.Writer = os.Stdout
	Stdin  io.Reader = os.Stdin
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
		nr, err := Stdin.Read(module.Bytes[addr : addr+n])
		module.SetI32(written, int32(nr))
		if err == nil {
			return 0
		}
		return 1
	}
	f, o := files[fd]
	if o == false {
		return 1
	}
	copy(module.Bytes[addr:], f.b[:n])
	f.b = f.b[n:]
	files[fd] = f
	module.SetI32(written, n)
	return 0
}
func Fd_write(fd, p, niovec, written int32) int32 {
	addr := module.I32(p)
	n := module.I32(4 + p)
	b := module.Bytes[addr : addr+n]
	if fd == 1 {
		nw, err := Stdout.Write(b)
		module.SetI32(written, int32(nw))
		if err == nil {
			return 0
		}
	} else {
		f, o := files[fd]
		if o == false {
			return 1
		}
		f.b = append(f.b, b...)
		files[fd] = f
		module.SetI32(written, int32(len(b)))
		return 0
	}
	return 1
}
func Fd_seek(fp int32, offset int64, whence int32, rp int32) int32 {
	f, o := files[fp]
	if o == false {
		return 1
	}
	if whence == 2 {
		module.SetI32(rp, int32(len(f.b)))
	} else {
		module.SetI32(rp, 0)
	}
	return 0
}

type openfile struct {
	name string
	b    []byte
}

var files map[int32]openfile

func Path_open(fd, dirflags, path, pathlen, oflags int32, baserights, inheritrights int64, fdflags, newfp int32) int32 {
	fp := int32(2 + len(files))
	name := string(module.Bytes[path : path+pathlen])
	if oflags == 0 { // assume read
		b, e := ioutil.ReadFile(name)
		if e != nil {
			fmt.Println(e)
			return 1
		}
		files[fp] = openfile{b: b}
	} else { // assume write file
		files[fp] = openfile{name: name, b: make([]byte, 0)}
	}
	module.SetI32(newfp, fp)
	return 0
}

func Fd_close(x int32) int32 {
	f, o := files[x]
	if o == false {
		return 1
	}
	if f.name != "" {
		if os.WriteFile(f.name, f.b, 0644) != nil {
			return 1
		}
	}
	delete(files, x)
	return 0
}
func Proc_exit(x int32) { os.Exit(int(x)) }

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
	files = make(map[int32]openfile)
}
