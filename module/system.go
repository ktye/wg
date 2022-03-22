package module

import (
	"io"
	"os"
)

var ( // only for testing
	Stdout io.Writer = os.Stdout
	Stdin  io.Reader = os.Stdin
)

func Exit(x int32) { os.Exit(int(x)) }

func Args() int32 { return int32(len(os.Args)) }

// Arg(i, 0) => length
// allocate..
// Arg(i, dst) => copy
func Arg(i, r int32) int32 {
	if i >= int32(len(os.Args)) {
		return 0
	}
	if r == 0 {
		return int32(len(os.Args[i]))
	}
	copy(Bytes[r:], []byte(os.Args[i]))
	return 0
}

var filebuf []byte

// Read(file, length_of_filename, 0) => filesize
// <0: error, e.g. not exist
// allocate..
// Read(file, length_of_filename, dst) => copy
func Read(file, nfile, dst int32) int32 {
	if dst != 0 {
		copy(Bytes[dst:], filebuf)
		return 0
	}
	b, e := os.ReadFile(string(Bytes[file : file+nfile]))
	if e != nil {
		return -1
	}
	filebuf = b
	return int32(len(filebuf))
}
func Write(file, nfile, src, n int32) int32 {
	b := Bytes[src : src+n]
	if nfile == 0 {
		Stdout.Write(b)
		return 0
	}
	name := string(Bytes[file : file+nfile])
	e := os.WriteFile(name, b, 0644)
	if e != nil {
		return -1
	}
	return 0
}
func ReadIn(dst, n int32) int32 {
	nr, e := Stdin.Read(Bytes[dst : dst+n])
	if e != nil {
		return 0
	}
	return int32(nr)
}
