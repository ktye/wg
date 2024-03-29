package module

import (
	"encoding/binary"
	"fmt"
	"math"
	"math/bits"
	"os"
)

// Memory creates a linear memory with size blocks*64kB.
func Memory(blocks int) {
	if Bytes == nil {
		Bytes = make([]byte, 64*1024*blocks)
	}
}
func Memory2(blocks int) {
	if Bytes2 == nil {
		Bytes2 = make([]byte, 64*1024*blocks)
	}
}
func Memorysize() int32  { return int32(len(Bytes) >> 16) }
func Memorysize2() int32 { return int32(len(Bytes2) >> 16) }
func Memorygrow(blocks int32) (previous int32) {
	previous = int32(len(Bytes) >> 16)
	delta := 64 * 1024 * int(blocks)
	if len(Bytes)+delta > (1<<32 - 1) {
		return -1
	}
	Bytes = append(Bytes, make([]byte, delta)...)
	return previous
}
func Memorygrow2(blocks int32) int32 {
	Bytes, Bytes2 = Bytes2, Bytes
	r := Memorygrow(blocks)
	Bytes, Bytes2 = Bytes2, Bytes
	return r
}

var Bytes []byte
var Bytes2 []byte

func I8(addr int32) int32           { return int32(int8(Bytes[addr])) }
func U8(addr int32) uint32          { return uint32(Bytes[addr]) }
func SetI8(addr int32, value int32) { Bytes[addr] = byte(value) }

func I16(addr int32) int32 {
	aln(addr, 2)
	return int32(int16(binary.LittleEndian.Uint16(Bytes[addr:])))
}
func U16(addr int32) uint32 { aln(addr, 2); return uint32(binary.LittleEndian.Uint16(Bytes[addr:])) }
func I32(addr int32) int32  { aln(addr, 4); return int32(binary.LittleEndian.Uint32(Bytes[addr:])) }
func U32(addr int32) uint32 { aln(addr, 4); return binary.LittleEndian.Uint32(Bytes[addr:]) }
func I64(addr int32) int64  { aln(addr, 8); return int64(binary.LittleEndian.Uint64(Bytes[addr:])) }
func U64(addr int32) uint64 { aln(addr, 8); return binary.LittleEndian.Uint64(Bytes[addr:]) }
func F32(addr int32) float32 {
	aln(addr, 4)
	return math.Float32frombits(binary.LittleEndian.Uint32(Bytes[addr:]))
}
func F64(addr int32) float64 {
	aln(addr, 8)
	return math.Float64frombits(binary.LittleEndian.Uint64(Bytes[addr:]))
}
func SetI16(addr int32, value int32) {
	aln(addr, 2)
	binary.LittleEndian.PutUint16(Bytes[addr:], uint16(value))
}
func SetI32(addr int32, value int32) {
	aln(addr, 4)
	binary.LittleEndian.PutUint32(Bytes[addr:], uint32(value))
}
func SetI64(addr int32, value int64) {
	aln(addr, 8)
	binary.LittleEndian.PutUint64(Bytes[addr:], uint64(value))
}
func SetF32(addr int32, value float32) {
	aln(addr, 4)
	binary.LittleEndian.PutUint32(Bytes[addr:], math.Float32bits(value))
}
func SetF64(addr int32, value float64) {
	aln(addr, 8)
	binary.LittleEndian.PutUint64(Bytes[addr:], math.Float64bits(value))
}

const aligncheck = true

func aln(addr int32, a int32) {
	if aligncheck && (addr&(a-1) != 0) {
		panic(fmt.Sprintf("non-aligned memory access addr=%d (%d)", addr, a))
	}
}

/* unsafe variants
func I16(addr int32) int32             { return int32(*(*int16)(unsafe.Pointer(&Bytes[addr]))) }
func U16(addr int32) uint32            { return uint32(*(*uint16)(unsafe.Pointer(&Bytes[addr]))) }
func I32(addr int32) int32             { return *(*int32)(unsafe.Pointer(&Bytes[addr])) }
func U32(addr int32) uint32            { return *(*uint32)(unsafe.Pointer(&Bytes[addr])) }
func I64(addr int32) int64             { return *(*int64)(unsafe.Pointer(&Bytes[addr])) }
func U64(addr int32) uint64            { return *(*uint64)(unsafe.Pointer(&Bytes[addr])) }
func F32(addr int32) float32           { return *(*float32)(unsafe.Pointer(&Bytes[addr])) }
func F64(addr int32) float64           { return *(*float64)(unsafe.Pointer(&Bytes[addr])) }
func SetI16(addr int32, value int32)   { *(*int16)(unsafe.Pointer(&Bytes[addr])) = int16(value) }
func SetI32(addr int32, value int32)   { *(*int32)(unsafe.Pointer(&Bytes[addr])) = value }
func SetI64(addr int32, value int64)   { *(*int64)(unsafe.Pointer(&Bytes[addr])) = value }
func SetF32(addr int32, value float32) { *(*float32)(unsafe.Pointer(&Bytes[addr])) = value }
func SetF64(addr int32, value float64) { *(*float64)(unsafe.Pointer(&Bytes[addr])) = value }
*/

// Func is the indirect function table. Calling needs to do a type assertion.
var Func []interface{}

// Functions adds the function arguments to the indirect function table
// starting at the given offset.
func Functions(off int, funcs ...interface{}) {
	if n := off + len(funcs); n >= len(Func) {
		Func = append(Func, make([]interface{}, n-len(Func))...)
	}
	for i, f := range funcs {
		Func[i+off] = f
	}
}

// Export registers the functions as exported.
func Export(funcs ...interface{}) {}
func ExportAll()                  {}

// Data section (call after Memory)
func Data(off int, value string) { copy(Bytes[off:], value) }

// Bulk memory instructions.
func Memorycopy(dst, src, n int32) { copy(Bytes[dst:], Bytes[src:src+n]) }
func Memoryfill(dst, val, n int32) {
	b := byte(val)
	for i := int32(0); i < n; i++ {
		Bytes[dst+i] = b
	}
}
func Memorycopy2(dst, src, n int32) { copy(Bytes2[dst:], Bytes[src:src+n]) }
func Memorycopy3(dst, src, n int32) { copy(Bytes[dst:], Bytes2[src:src+n]) }

func I32B(b bool) int32 {
	if b {
		return 1
	}
	return 0
}

// Exception-handling
func Catch(f func()) {
	if recover() != nil {
		f()
	}
}

// Direct wasm opcodes
func I32clz(x int32) int32     { return int32(bits.LeadingZeros32(uint32(x))) }
func I64clz(x uint64) int64    { return int64(bits.LeadingZeros64(x)) }
func I32ctz(x uint32) int32    { return int32(bits.TrailingZeros32(x)) }
func I64ctz(x uint64) int64    { return int64(bits.TrailingZeros64(x)) }
func I32popcnt(x uint32) int32 { return int32(bits.OnesCount32(x)) }
func I64popcnt(x uint64) int64 { return int64(bits.OnesCount64(x)) }

// f64 ops
func F64abs(x float64) float64         { return math.Abs(x) }
func F64sqrt(x float64) float64        { return math.Sqrt(x) }
func F64ceil(x float64) float64        { return math.Ceil(x) }
func F64floor(x float64) float64       { return math.Floor(x) }
func F64nearest(x float64) float64     { return math.Round(x) }
func F64min(x, y float64) float64      { return math.Min(x, y) }
func F64max(x, y float64) float64      { return math.Max(x, y) }
func F64copysign(x, y float64) float64 { return math.Copysign(x, y) }

func F32reinterpret_i32(x uint32) float32 { return math.Float32frombits(x) }
func F64reinterpret_i64(x uint64) float64 { return math.Float64frombits(x) }
func I32reinterpret_f32(x float32) uint32 { return math.Float32bits(x) }
func I64reinterpret_f64(x float64) uint64 { return math.Float64bits(x) }

// for debugging only (ignored by wasm)
// func Printf(f string, a ...interface{}) { fmt.Printf(f, a...) }
func Prinf(x int32, y float64) { fmt.Fprintf(os.Stderr, "%d %.6f\n", x, y) }
func Printi(x, y int32)        { fmt.Fprintf(os.Stderr, "%d %d\n", x, y) }
func Printk(x int32, y uint64) {
	fmt.Fprintf(os.Stderr, "%d ", x)
	printk(y)
	fmt.Fprintf(os.Stderr, "\n")
}
func printk(y uint64) {
	y &^= 1
	t := y >> 59
	if t > 16 {
		n := I32(int32(y) - 12)
		fmt.Fprintf(os.Stderr, "(%d#%d) ", n, y>>59)
	} else {
		fmt.Fprintf(os.Stderr, "(%d) ", y>>59)
	}
}
func Printu(x int32, y uint64) { fmt.Fprintf(os.Stderr, "%d %d\n", x, y) }
func Printl(x int32, y uint64) {
	if 23 != y>>59 {
		fmt.Fprintf(os.Stderr, "%dL !%d\n", x, y>>59)
		return
	}
	y &^= 1
	n := I32(int32(y) - 12)
	fmt.Fprintf(os.Stderr, "%dL %d# ", x, n)
	if n > 5 {
		n = 5
	}
	for i := int32(0); i < n; i++ {
		yp := int32(y) + 8*i
		printk(uint64(I64(yp)))
	}
	fmt.Fprintf(os.Stderr, "\n")
}
