package module

import "math"

// Simd v128
type V128 interface{}

func I8x16load(addr int32) V128 {
	var b [16]int8
	for i := int32(0); i < 16; i++ {
		b[i] = int8(Bytes[addr+i])
	}
	return b
}
func I32x4load(addr int32) V128 {
	return [4]int32{I32(addr), I32(4 + addr), I32(8 + addr), I32(12 + addr)}
}
func I64x2load(addr int32) V128 { return [2]int64{I64(addr), I64(8 + addr)} }
func F32x4load(addr int32) V128 {
	return [4]float32{F32(addr), F32(4 + addr), F32(8 + addr), F32(12 + addr)}
}
func F64x2load(addr int32) V128 { return [2]float64{F64(addr), F64(8 + addr)} }

func I8x16store(addr int32, v V128) { b := v.([16]byte); copy(Bytes[addr:], b[:]) }
func I32x4store(addr int32, v V128) {
	b := v.([4]int32)
	for i := range b {
		SetI32(addr, b[i])
		addr += 4
	}
}
func F32x4store(addr int32, v V128) {
	b := v.([4]float32)
	for i := range b {
		SetF32(addr, b[i])
		addr += 4
	}
}
func I64x2store(addr int32, v V128) {
	b := v.([2]int64)
	SetI64(addr, b[0])
	SetI64(8+addr, b[1])
}
func F64x2store(addr int32, v V128) {
	b := v.([2]float64)
	SetF64(addr, b[0])
	SetF64(8+addr, b[1])
}

func I8x16splat(x int32) V128 {
	var r [16]int8
	for i := range r {
		r[i] = int8(x)
	}
	return r
}
func I32x4splat(x int32) V128   { return [4]int32{x, x, x, x} }
func I64x2splat(x int64) V128   { return [2]int64{x, x} }
func F32x4splat(x float32) V128 { return [4]float32{x, x, x, x} }
func F64x2splat(x float64) V128 { return [2]float64{x, x} }

func mini8(x, y int8) int8    { return int8(mini64(int64(x), int64(y))) }
func mini32(x, y int32) int32 { return int32(mini64(int64(x), int64(y))) }
func mini64(x, y int64) int64 {
	if x < y {
		return x
	} else {
		return y
	}
}
func maxi8(x, y int8) int8    { return int8(maxi64(int64(x), int64(y))) }
func maxi32(x, y int32) int32 { return int32(maxi64(int64(x), int64(y))) }
func maxi64(x, y int64) int64 {
	if x > y {
		return x
	} else {
		return y
	}
}
func absi8(x int8) int8    { return int8(absi64(int64(x))) }
func absi32(x int32) int32 { return int32(absi64(int64(x))) }
func absi64(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}

func I8x16all_true(x V128) int32 {
	a := x.([16]int8)
	for i := range a {
		if a[i] != 0 {
			return 0
		}
	}
	return 1
}
func I8x16any_true(x V128) int32 {
	a := x.([16]int8)
	for i := range a {
		if a[i] != 0 {
			return 1
		}
	}
	return 0
}

func I8x16neg(x V128) V128 {
	var r [16]int8
	a := x.([16]int8)
	for i := range r {
		r[i] = -a[i]
	}
	return r
}
func I8x16abs(x V128) V128 {
	var r [16]int8
	a := x.([16]int8)
	for i := range r {
		r[i] = absi8(a[i])
	}
	return r
}
func I8x16add(x, y V128) V128 {
	var r [16]int8
	a, b := x.([16]int8), y.([16]int8)
	for i := range r {
		r[i] = a[i] + b[i]
	}
	return r
}
func I8x16sub(x, y V128) V128 {
	var r [16]int8
	a, b := x.([16]int8), y.([16]int8)
	for i := range r {
		r[i] = a[i] - b[i]
	}
	return r
}
func I8x16mul(x, y V128) V128 {
	var r [16]int8
	a, b := x.([16]int8), y.([16]int8)
	for i := range r {
		r[i] = a[i] * b[i]
	}
	return r
}
func I8x16min_s(x, y V128) V128 {
	var r [16]int8
	a, b := x.([16]int8), y.([16]int8)
	for i := range r {
		r[i] = mini8(a[i], b[i])
	}
	return r
}
func I8x16max_s(x, y V128) V128 {
	var r [16]int8
	a, b := x.([16]int8), y.([16]int8)
	for i := range r {
		r[i] = maxi8(a[i], b[i])
	}
	return r
}

func I32x4neg(x V128) V128 {
	var r [4]int32
	a := x.([4]int32)
	for i := range r {
		r[i] = -a[i]
	}
	return r
}
func I32x4abs(x V128) V128 {
	var r [4]int32
	a := x.([4]int32)
	for i := range r {
		r[i] = absi32(a[i])
	}
	return r
}
func I32x4add(x, y V128) V128 {
	var r [4]int32
	a, b := x.([4]int32), y.([4]int32)
	for i := range r {
		r[i] = a[i] + b[i]
	}
	return r
}
func I32x4sub(x, y V128) V128 {
	var r [4]int32
	a, b := x.([4]int32), y.([4]int32)
	for i := range r {
		r[i] = a[i] - b[i]
	}
	return r
}
func I32x4mul(x, y V128) V128 {
	var r [4]int32
	a, b := x.([4]int32), y.([4]int32)
	for i := range r {
		r[i] = a[i] * b[i]
	}
	return r
}
func I32x4min_s(x, y V128) V128 {
	var r [4]int32
	a, b := x.([4]int32), y.([4]int32)
	for i := range r {
		r[i] = mini32(a[i], b[i])
	}
	return r
}
func I32x4max_s(x, y V128) V128 {
	var r [4]int32
	a, b := x.([4]int32), y.([4]int32)
	for i := range r {
		r[i] = maxi32(a[i], b[i])
	}
	return r
}

func I64x2neg(x V128) V128 {
	var r [2]int64
	a := x.([2]int64)
	for i := range r {
		r[i] = -a[i]
	}
	return r
}
func I64x2abs(x V128) V128 {
	var r [2]int64
	a := x.([2]int64)
	for i := range r {
		r[i] = absi64(a[i])
	}
	return r
}
func I64x2add(x, y V128) V128 {
	var r [2]int64
	a, b := x.([2]int64), y.([2]int64)
	r[0] = a[0] + b[0]
	r[1] = a[1] + b[1]
	return r
}
func I64x2sub(x, y V128) V128 {
	var r [2]int64
	a, b := x.([2]int64), y.([2]int64)
	r[0] = a[0] - b[0]
	r[1] = a[1] - b[1]
	return r
}
func I64x2mul(x, y V128) V128 {
	var r [2]int64
	a, b := x.([2]int64), y.([2]int64)
	r[0] = a[0] * b[0]
	r[1] = a[1] * b[1]
	return r
}
func I64x2min_s(x, y V128) V128 {
	var r [2]int64
	a, b := x.([2]int64), y.([2]int64)
	r[0] = mini64(a[0], b[0])
	r[1] = mini64(a[1], b[1])
	return r
}
func I64x2max_s(x, y V128) V128 {
	var r [2]int64
	a, b := x.([2]int64), y.([2]int64)
	r[0] = maxi64(a[0], b[0])
	r[1] = maxi64(a[1], b[1])
	return r
}

func F64x2add(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	r[0] = a[0] + b[0]
	r[1] = a[1] + b[1]
	return r
}
func F64x2sub(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	r[0] = a[0] - b[0]
	r[1] = a[1] - b[1]
	return r
}
func F64x2mul(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	r[0] = a[0] * b[0]
	r[1] = a[1] * b[1]
	return r
}
func F64x2div(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	r[0] = a[0] / b[0]
	r[1] = a[1] / b[1]
	return r
}
func F64x2min(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	r[0] = math.Min(a[0], b[0])
	r[1] = math.Min(a[1], b[1])
	return r
}
func F64x2max(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	r[0] = math.Max(a[0], b[0])
	r[1] = math.Max(a[1], b[1])
	return r
}

func I8x16eq(x, y V128) V128 {
	var r [16]int8
	a, b := x.([16]int8), y.([16]int8)
	for i := range r {
		if a[i] == b[i] {
			r[i] = -1
		}
	}
	return r
}
func I8x16ne(x, y V128) V128 {
	var r [16]int8
	a, b := x.([16]int8), y.([16]int8)
	for i := range r {
		if a[i] != b[i] {
			r[i] = -1
		}
	}
	return r
}
func I8x16lt_s(x, y V128) V128 {
	var r [16]int8
	a, b := x.([16]int8), y.([16]int8)
	for i := range r {
		if a[i] < b[i] {
			r[i] = -1
		}
	}
	return r
}
func I8x16gt_s(x, y V128) V128 {
	var r [16]int8
	a, b := x.([16]int8), y.([16]int8)
	for i := range r {
		if a[i] > b[i] {
			r[i] = -1
		}
	}
	return r
}

func I32x4eq(x, y V128) V128 {
	var r [4]int32
	a, b := x.([4]int32), y.([4]int32)
	for i := range r {
		if a[i] == b[i] {
			r[i] = -1
		}
	}
	return r
}
func I32x4ne(x, y V128) V128 {
	var r [4]int32
	a, b := x.([4]int32), y.([4]int32)
	for i := range r {
		if a[i] == b[i] {
			r[i] = -1
		}
	}
	return r
}
func I32x4lt_s(x, y V128) V128 {
	var r [4]int32
	a, b := x.([4]int32), y.([4]int32)
	for i := range r {
		if a[i] < b[i] {
			r[i] = -1
		}
	}
	return r
}
func I32x4gt_s(x, y V128) V128 {
	var r [4]int32
	a, b := x.([4]int32), y.([4]int32)
	for i := range r {
		if a[i] > b[i] {
			r[i] = -1
		}
	}
	return r
}

func I64x2eq(x, y V128) V128 {
	var r [2]int64
	a, b := x.([2]int64), y.([2]int64)
	for i := range r {
		if a[i] == b[i] {
			r[i] = -1
		}
	}
	return r
}
func I64x2ne(x, y V128) V128 {
	var r [2]int64
	a, b := x.([2]int64), y.([2]int64)
	for i := range r {
		if a[i] == b[i] {
			r[i] = -1
		}
	}
	return r
}
func I64x2lt_s(x, y V128) V128 {
	var r [2]int64
	a, b := x.([2]int64), y.([2]int64)
	for i := range r {
		if a[i] < b[i] {
			r[i] = -1
		}
	}
	return r
}
func I64x2gt_s(x, y V128) V128 {
	var r [2]int64
	a, b := x.([2]int64), y.([2]int64)
	for i := range r {
		if a[i] > b[i] {
			r[i] = -1
		}
	}
	return r
}

func F64x2eq(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	for i := range r {
		if a[i] == b[i] {
			r[i] = -1
		}
	}
	return r
}
func F64x2ne(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	for i := range r {
		if a[i] == b[i] {
			r[i] = -1
		}
	}
	return r
}
func F64x2lt(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	for i := range r {
		if a[i] < b[i] {
			r[i] = -1
		}
	}
	return r
}
func F64x2gt(x, y V128) V128 {
	var r [2]float64
	a, b := x.([2]float64), y.([2]float64)
	for i := range r {
		if a[i] > b[i] {
			r[i] = -1
		}
	}
	return r
}
