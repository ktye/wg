package module

import (
	"encoding/binary"
	"math"
)

// Memory creates a linear memory with size blocks*64kB.
func Memory(blocks int) { Bytes = make([]byte, 64*1024*blocks) }

var Bytes []byte

func I8(addr int32) int32    { return int32(int8(Bytes[addr])) }
func U8(addr int32) uint32   { return uint32(Bytes[addr]) }
func I16(addr int32) int32   { return int32(int16(binary.LittleEndian.Uint16(Bytes[addr:]))) }
func U16(addr int32) uint32  { return uint32(binary.LittleEndian.Uint16(Bytes[addr:])) }
func I32(addr int32) int32   { return int32(binary.LittleEndian.Uint16(Bytes[addr:])) }
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
	if n := off + len(Func); n >= len(Func) {
		Func = append(Func, make([]interface{}, 1+len(Func)-n)...)
	}
	for i, f := range funcs {
		Func[i+off] = f
	}
}

// Export registers the functions as exported.
func Export(funcs ...interface{}) {}
