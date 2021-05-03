package module

import (
	"unsafe"
)

// M is the linear memory space of the module.
// Use one of Memory or ImportMemory to create it.
// I16..F64 are slices sharing the same underlying array.
// Memory can be accessed using the slices with array indexing,
// or using the corresponding Get/Set functions with byte addresses.
var M []byte
var I16 []int16
var I32 []int32
var I64 []int64
var U16 []uint16
var U32 []uint32
var U64 []uint64
var F32 []float32
var F64 []float64

// Memory creates a linear memory with size blocks*64kB.
func Memory(blocks int) { M = make([]byte, 64*1024*blocks); msl() }

// ImportMemory uses the provided memory at the modules linear memory.
func ImportMemory(m []byte) { M = m }

func GetI16(addr int32) int16   { return I16[addr>>1] }
func GetI32(addr int32) int32   { return I32[addr>>2] }
func GetI64(addr int32) int64   { return I64[addr>>3] }
func GetU16(addr int32) uint16  { return U16[addr>>1] }
func GetU32(addr int32) uint32  { return U32[addr>>2] }
func GetU64(addr int32) uint64  { return U64[addr>>3] }
func GetF32(addr int32) float32 { return F32[addr>>2] }
func GetF64(addr int32) float64 { return F64[addr>>3] }

func SetI16(addr int32, x int16)   { I16[addr>>1] = x }
func SetI32(addr int32, x int32)   { I32[addr>>2] = x }
func SetI64(addr int32, x int64)   { I64[addr>>3] = x }
func SetU16(addr int32, x uint16)  { U16[addr>>1] = x }
func SetU32(addr int32, x uint32)  { U32[addr>>2] = x }
func SetU64(addr int32, x uint64)  { U64[addr>>3] = x }
func SetF32(addr int32, x float32) { F32[addr>>2] = x }
func SetF64(addr int32, x float64) { F64[addr>>3] = x }

// F is the indirect function table. Call need to do a type
var F []interface{}

// Functions adds the function arguments to the indirect function table
// starting at the given offset.
func Functions(off int, funcs ...interface{}) {
	if n := off + len(F); n >= len(F) {
		F = append(F, make([]interface{}, 1+len(F)-n)...)
	}
	for i, f := range funcs {
		F[i+off] = f
	}
}

// msl creates slices with underlying shared memory afer updating M.
func msl() {
	type slice struct {
		p uintptr
		l int
		c int
	}
	m := *(*slice)(unsafe.Pointer(&M))
	i16 := *(*slice)(unsafe.Pointer(&I16))
	i32 := *(*slice)(unsafe.Pointer(&I32))
	i64 := *(*slice)(unsafe.Pointer(&I64))
	u16 := *(*slice)(unsafe.Pointer(&U16))
	u32 := *(*slice)(unsafe.Pointer(&U32))
	u64 := *(*slice)(unsafe.Pointer(&U64))
	f32 := *(*slice)(unsafe.Pointer(&F32))
	f64 := *(*slice)(unsafe.Pointer(&F64))
	i16.l, i16.c, i16.p = m.l/2, m.c/2, m.p
	i32.l, i32.c, i32.p = m.l/4, m.c/4, m.p
	i64.l, i64.c, i64.p = m.l/8, m.c/8, m.p
	u32.l, u32.c, u32.p = m.l/4, m.c/4, m.p
	u64.l, u64.c, u64.p = m.l/8, m.c/8, m.p
	f32.l, f32.c, f32.p = m.l/4, m.c/4, m.p
	f64.l, f64.c, f64.p = m.l/8, m.c/8, m.p
	I16 = *(*[]int16)(unsafe.Pointer(&i16))
	I32 = *(*[]int32)(unsafe.Pointer(&i32))
	I64 = *(*[]int64)(unsafe.Pointer(&i64))
	U16 = *(*[]uint16)(unsafe.Pointer(&u16))
	U32 = *(*[]uint32)(unsafe.Pointer(&u32))
	U64 = *(*[]uint64)(unsafe.Pointer(&u64))
	F32 = *(*[]float32)(unsafe.Pointer(&f32))
	F64 = *(*[]float64)(unsafe.Pointer(&f64))
}
