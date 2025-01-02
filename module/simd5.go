package module

import "math"

// gcc 32byte vector extensions(avx2..)
type C5 [32]int8
type I5 [8]int32
type F5 [4]float64

func Iota5() I5 { return I5{0, 1, 2, 3, 4, 5, 6, 7} }
func C5load(addr int32) (r C5) {
	for i := int32(0); i < 32; i++ {
		r[i] = int8(Bytes[addr+i])
	}
	return r
}
func I5load(addr int32) I5 { return I5{ I32(addr), I32(4 + addr), I32(8 + addr), I32(12 + addr), I32(16 + addr), I32(20 + addr), I32(24 + addr), I32(28 + addr)} }
func F5load(addr int32) F5 { return F5{ F64(addr), F64(8 + addr), F64(16+addr), F64(24 + addr) } }

func C5store(addr int32, v C5) {
	for i := range v {
		Bytes[addr+int32(i)] = byte(v[i])
	}
}
func I5store(addr int32, v I5) {
	for i := range v {
		SetI32(addr, v[i])
		addr += 4
	}
}
func F5store(addr int32, v F5) {
	SetF64(addr, v[0])
	SetF64(8+addr, v[1])
	SetF64(16+addr, v[2])
	SetF64(24+addr, v[3])
}

func C5splat(x int32) (r C5) {
	for i := range r {
		r[i] = int8(x)
	}
	return r
}
func I5splat(x int32) (r I5)   { return I5{x, x, x, x, x, x, x, x} }
func F5splat(x float64) (r F5) { return F5{x, x, x, x} }

func (v C5) Neg() (r C5) {
	for i := range r {
		r[i] = -v[i]
	}
	return r
}
func (v C5) Abs() (r C5) {
	for i := range r {
		r[i] = absi8(v[i])
	}
	return r
}
func (x C5) Add(y C5) (r C5) {
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x C5) Sub(y C5) (r C5) {
	for i := range r {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x C5) Mul(y C5) (r C5) {
	for i := range r {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x C5) Shl(y int32) (r C5) {
	for i := range r {
		r[i] = x[i] << y
	}
	return r
}
func (x C5) Shr_s(y int32) (r C5) {
	for i := range r {
		r[i] = x[i] >> y
	}
	return r
}
func (x C5) Shr_u(y int32) (r C5) {
	for i := range r {
		r[i] = int8(uint8(x[i])) >> y
	}
	return r
}
func (x C5) Min_s(y C5) (r C5) {
	for i := range r {
		r[i] = mini8(x[i], y[i])
	}
	return r
}
func (x C5) Max_s(y C5) (r C5) {
	for i := range r {
		r[i] = maxi8(x[i], y[i])
	}
	return r
}
func (x C5) And(y C5) (r C5) {
	for i := range r {
		r[i] = x[i] & y[i]
	}
	return r
}
func (x C5) Not() (r C5) {
	for i := range r {
		r[i] = ^x[i]
	}
	return r
}

func (v I5) Neg() (r I5) {
	for i := range r {
		r[i] = -v[i]
	}
	return r
}
func (v I5) Abs() (r I5) {
	for i := range r {
		r[i] = absi32(v[i])
	}
	return r
}
func (x I5) Add(y I5) (r I5) {
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x I5) Sub(y I5) (r I5) {
	for i := range r {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x I5) Mul(y I5) (r I5) {
	for i := range r {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x I5) Shl(y int32) (r I5) {
	for i := range r {
		r[i] = x[i] << y
	}
	return r
}
func (x I5) Shr_s(y int32) (r I5) {
	for i := range r {
		r[i] = x[i] >> y
	}
	return r
}
func (x I5) Shr_u(y int32) (r I5) {
	for i := range r {
		r[i] = int32(uint32(x[i])) >> y
	}
	return r
}
func (x I5) Min_s(y I5) (r I5) {
	for i := range r {
		r[i] = mini32(x[i], y[i])
	}
	return r
}
func (x I5) Max_s(y I5) (r I5) {
	for i := range r {
		r[i] = maxi32(x[i], y[i])
	}
	return r
}

func (v F5) Sqrt() (r F5) {
	for i := range v {
		r[i] = math.Sqrt(v[i])
	}
	return r
}
func (v F5) Abs() (r F5)  {
	for i := range v {
		r[i] = math.Abs(v[i])
	}
	return r
}
func (v F5) Neg() (r F5) {
	for i := range v {
		r[i] = -v[i]
	}
	return r
}
func (x F5) Add(y F5) (r F5) {
	for i := range x {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x F5) Sub(y F5) (r F5) {
	for i := range x {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x F5) Mul(y F5) (r F5) {
	for i := range x {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x F5) Div(y F5) (r F5) {
	for i := range x {
		r[i] = x[i] / y[i]
	}
	return r
}
func (x F5) Pmin(y F5) (r F5) {
	for i := range x {
		r[i] = pmin(x[i], y[i])
	}
	return r
}
func (x F5) Pmax(y F5) (r F5) {
	for i := range x {
		r[i] = pmax(x[i], y[i])
	}
	return r
}

func (x C5) Eq(y C5) (r C5) {
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x C5) Ne(y C5) (r C5) {
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x C5) Lt_u(y C5) (r C5) {
	for i := range r {
		if uint32(x[i]) < uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}
func (x C5) Lt_s(y C5) (r C5) {
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x C5) Gt_u(y C5) (r C5) {
	for i := range r {
		if uint32(x[i]) > uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}
func (x C5) Gt_s(y C5) (r C5) {
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}

func (x I5) Eq(y I5) (r I5) {
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I5) Ne(y I5) (r I5) {
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I5) Lt_s(y I5) (r I5) {
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I5) Lt_u(y I5) (r I5) {
	for i := range r {
		if uint32(x[i]) < uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}
func (x I5) Gt_s(y I5) (r I5) {
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I5) Gt_u(y I5) (r I5) {
	for i := range r {
		if uint32(x[i]) > uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}

func (x F5) Eq(y F5) (r F5) {
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x F5) Ne(y F5) (r F5) {
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x F5) Lt_s(y F5) (r F5) {
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x F5) Gt_s(y F5) (r F5) {
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}
