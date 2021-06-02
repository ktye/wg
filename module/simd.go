package module

import "math"

// Simd v128
type I8x16 [16]int8
type I32x4 [4]int32
type F64x2 [2]float64

func I8x16load(addr int32) (r I8x16) {
	for i := int32(0); i < 16; i++ {
		r[i] = int8(Bytes[addr+i])
	}
	return r
}
func I32x4load(addr int32) I32x4 {
	return I32x4{I32(addr), I32(4 + addr), I32(8 + addr), I32(12 + addr)}
}
func F64x2load(addr int32) F64x2 { return F64x2{F64(addr), F64(8 + addr)} }

func I8x16store(addr int32, v I8x16) {
	for i := range v {
		Bytes[addr+int32(i)] = byte(v[i])
	}
}
func I32x4store(addr int32, v I32x4) {
	for i := range v {
		SetI32(addr, v[i])
		addr += 4
	}
}
func F64x2store(addr int32, v F64x2) {
	SetF64(addr, v[0])
	SetF64(8+addr, v[1])
}

func I8x16splat(x int32) (r I8x16) {
	for i := range r {
		r[i] = int8(x)
	}
	return r
}
func I32x4splat(x int32) (r I32x4)   { return I32x4{x, x, x, x} }
func F64x2splat(x float64) (r F64x2) { return F64x2{x, x} }
func (v F64x2) Replace_lane1(f float64) F64x2 {
	v[1] = f
	return v
}
func (v F64x2) Shuffle() (r F64x2) { r[0], r[1] = v[1], v[0]; return r }

func mini8(x, y int8) int8 { return int8(mini32(int32(x), int32(y))) }
func mini32(x, y int32) int32 {
	if x < y {
		return x
	} else {
		return y
	}
}
func maxi8(x, y int8) int8 { return int8(maxi32(int32(x), int32(y))) }
func maxi32(x, y int32) int32 {
	if x > y {
		return x
	} else {
		return y
	}
}
func absi8(x int8) int8 { return int8(absi32(int32(x))) }
func absi32(x int32) int32 {
	if x < 0 {
		return -x
	}
	return x
}

func (v I8x16) All_true() int32 {
	for i := range v {
		if v[i] != 0 {
			return 0
		}
	}
	return 1
}
func (v I8x16) Any_true() int32 {
	for i := range v {
		if v[i] != 0 {
			return 1
		}
	}
	return 0
}
func (v I32x4) Any_true() int32 {
	for i := range v {
		if v[i] != 0 {
			return 1
		}
	}
	return 0
}
func (v I8x16) Extract_lane_s0() int32 { return int32(v[0]) }

func (v I8x16) Neg() (r I8x16) {
	for i := range r {
		r[i] = -v[i]
	}
	return r
}
func (v I8x16) Abs() (r I8x16) {
	for i := range r {
		r[i] = absi8(v[i])
	}
	return r
}
func (x I8x16) Add(y I8x16) (r I8x16) {
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x I8x16) Sub(y I8x16) (r I8x16) {
	for i := range r {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x I8x16) Mul(y I8x16) (r I8x16) {
	for i := range r {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x I8x16) Min_s(y I8x16) (r I8x16) {
	for i := range r {
		r[i] = mini8(x[i], y[i])
	}
	return r
}
func (x I8x16) Max_s(y I8x16) (r I8x16) {
	for i := range r {
		r[i] = maxi8(x[i], y[i])
	}
	return r
}

func (v I32x4) Neg() (r I32x4) {
	for i := range r {
		r[i] = -v[i]
	}
	return r
}
func (v I32x4) Abs() (r I32x4) {
	for i := range r {
		r[i] = absi32(v[i])
	}
	return r
}
func (x I32x4) Add(y I32x4) (r I32x4) {
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x I32x4) Sub(y I32x4) (r I32x4) {
	for i := range r {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x I32x4) Mul(y I32x4) (r I32x4) {
	for i := range r {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x I32x4) Min_s(y I32x4) (r I32x4) {
	for i := range r {
		r[i] = mini32(x[i], y[i])
	}
	return r
}
func (x I32x4) Max_s(y I32x4) (r I32x4) {
	for i := range r {
		r[i] = maxi32(x[i], y[i])
	}
	return r
}

func (v F64x2) Sqrt() F64x2 { return F64x2{math.Sqrt(v[0]), math.Sqrt(v[1])} }
func (v F64x2) Abs() F64x2  { return F64x2{math.Abs(v[0]), math.Abs(v[1])} }
func (v F64x2) Neg() F64x2  { return F64x2{-v[0], -v[1]} }
func (x F64x2) Add(y F64x2) (r F64x2) {
	r[0] = x[0] + y[0]
	r[1] = x[1] + y[1]
	return r
}
func (x F64x2) Sub(y F64x2) (r F64x2) {
	r[0] = x[0] - y[0]
	r[1] = x[1] - y[1]
	return r
}
func (x F64x2) Mul(y F64x2) (r F64x2) {
	r[0] = x[0] * y[0]
	r[1] = x[1] * y[1]
	return r
}
func (x F64x2) Div(y F64x2) (r F64x2) {
	r[0] = x[0] / y[0]
	r[1] = x[1] / y[1]
	return r
}
func (x F64x2) Pmin(y F64x2) (r F64x2) {
	r[0] = pmin(x[0], y[0])
	r[1] = pmin(x[1], y[1])
	return r
}
func (x F64x2) Pmax(y F64x2) (r F64x2) {
	r[0] = pmax(x[0], y[0])
	r[1] = pmax(x[1], y[1])
	return r
}
func pmin(x, y float64) float64 {
	if y < x {
		return y
	}
	return x
}
func pmax(x, y float64) float64 {
	if x < y {
		return y
	}
	return x
}

func (x I8x16) Eq(y I8x16) (r I8x16) {
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I8x16) Ne(y I8x16) (r I8x16) {
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I8x16) Lt_s(y I8x16) (r I8x16) {
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I8x16) Gt_s(y I8x16) (r I8x16) {
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}

func (x I32x4) Eq(y I32x4) (r I32x4) {
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I32x4) Ne(y I32x4) (r I32x4) {
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I32x4) Lt_s(y I32x4) (r I32x4) {
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I32x4) Gt_s(y I32x4) (r I32x4) {
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}

func (x F64x2) Eq(y F64x2) (r F64x2) {
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x F64x2) Ne(y F64x2) (r F64x2) {
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x F64x2) Lt_s(y F64x2) (r F64x2) {
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x F64x2) Gt_s(y F64x2) (r F64x2) {
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}
