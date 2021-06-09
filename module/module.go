package module

import (
	"encoding/binary"
	"math"
	"math/bits"
)

// Memory creates a linear memory with size blocks*64kB.
func Memory(blocks int) {
	if Bytes == nil {
		Bytes = make([]byte, 64*1024*blocks)
	}
}
func Memorysize() int32 { return int32(len(Bytes) >> 16) }
func Memorygrow(blocks int32) (previous int32) {
	previous = int32(len(Bytes) >> 16)
	Bytes = append(Bytes, make([]byte, 64*1024*blocks)...)
	return previous
}

var Bytes []byte

func I8(addr int32) int32    { return int32(int8(Bytes[addr])) }
func U8(addr int32) uint32   { return uint32(Bytes[addr]) }
func I16(addr int32) int32   { return int32(int16(binary.LittleEndian.Uint16(Bytes[addr:]))) }
func U16(addr int32) uint32  { return uint32(binary.LittleEndian.Uint16(Bytes[addr:])) }
func I32(addr int32) int32   { return int32(binary.LittleEndian.Uint32(Bytes[addr:])) }
func U32(addr int32) uint32  { return binary.LittleEndian.Uint32(Bytes[addr:]) }
func I64(addr int32) int64   { return int64(binary.LittleEndian.Uint64(Bytes[addr:])) }
func U64(addr int32) uint64  { return binary.LittleEndian.Uint64(Bytes[addr:]) }
func F32(addr int32) float32 { return math.Float32frombits(binary.LittleEndian.Uint32(Bytes[addr:])) }
func F64(addr int32) float64 { return math.Float64frombits(binary.LittleEndian.Uint64(Bytes[addr:])) }

func SetI8(addr int32, value int32)  { Bytes[addr] = byte(value) }
func SetI16(addr int32, value int32) { binary.LittleEndian.PutUint16(Bytes[addr:], uint16(value)) }
func SetI32(addr int32, value int32) { binary.LittleEndian.PutUint32(Bytes[addr:], uint32(value)) }
func SetI64(addr int32, value int64) { binary.LittleEndian.PutUint64(Bytes[addr:], uint64(value)) }
func SetF32(addr int32, value float32) {
	binary.LittleEndian.PutUint32(Bytes[addr:], math.Float32bits(value))
}
func SetF64(addr int32, value float64) {
	binary.LittleEndian.PutUint64(Bytes[addr:], math.Float64bits(value))
}

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

// Direct wasm opcodes
func I32clz(x uint32) int32    { return int32(bits.LeadingZeros32(x)) }
func I64clz(x uint64) int32    { return int32(bits.LeadingZeros64(x)) }
func I32ctz(x uint32) int32    { return int32(bits.TrailingZeros32(x)) }
func I64ctz(x uint64) int32    { return int32(bits.TrailingZeros64(x)) }
func I32popcnt(x uint32) int32 { return int32(bits.OnesCount32(x)) }
func I64popcnt(x uint64) int32 { return int32(bits.OnesCount64(x)) }

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
