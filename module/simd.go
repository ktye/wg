package module

import (
	"math"
)

// wasm simd128

var nc, ni, nf int

func Simd(s int) {
	if s == 4 {
		nc, ni, nf = 16, 4, 2
	} else if s == 5 {
		nc, ni, nf = 32, 8, 4
	} else {
		panic("simd must be 4 or 5")
	}
}

type VC []int8
type VI []int32
type VF []float64

func mkC() VC { return make(VC, nc) }
func mkI() VI { return make(VI, ni) }
func mkF() VF { return make(VF, nf) }

func Iota() VI {
	r := mkI()
	for i := range r {
		r[i] = int32(i)
	}
	return r
}
func VCload(a int32) VC {
	r := mkC()
	for i := range r {
		r[i] = int8(Bytes[a+int32(i)])
	}
	return r
}
func VI1() VI {
	r := mkI()
	for i := range r {
		r[i] = 1
	}
	return r
}
func VIload(a int32) VI {
	r := mkI()
	for i := range r {
		r[i] = I32(a + 4*int32(i))
	}
	return r
}
func VFload(a int32) VF {
	r := mkF()
	for i := range r {
		r[i] = F64(a + 8*int32(i))
	}
	return r
}
func VIloadB(a int32) VI {
	r := mkI()
	for i := range r {
		r[i] = I8(a + int32(i))
	}
	return r
}
func VCstore(a int32, v VC) {
	for i := range v {
		Bytes[a+int32(i)] = byte(v[i])
	}
}
func VIstore(a int32, v VI) {
	for i := range v {
		SetI32(a, v[i])
		a += 4
	}
}
func VFstore(a int32, v VF) {
	for i := range v {
		SetF64(a, v[i])
		a += 8
	}
}
func VCsplat(x int32) VC {
	r := mkC()
	for i := range r {
		r[i] = int8(x)
	}
	return r
}
func VIsplat(x int32) VI {
	r := mkI()
	for i := range r {
		r[i] = x
	}
	return r
}
func VFsplat(x float64) VF {
	r := mkF()
	for i := range r {
		r[i] = x
	}
	return r
}
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

func (v VC) Neg() VC {
	r := mkC()
	for i := range r {
		r[i] = -v[i]
	}
	return r
}
func (v VC) Abs() VC {
	r := mkC()
	for i := range r {
		r[i] = absi8(v[i])
	}
	return r
}
func (x VC) Add(y VC) VC {
	r := mkC()
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x VC) Sub(y VC) VC {
	r := mkC()
	for i := range r {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x VC) Mul(y VC) VC {
	r := mkC()
	for i := range r {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x VC) Shl(y int32) VC {
	r := mkC()
	for i := range r {
		r[i] = x[i] << y
	}
	return r
}
func (x VC) Shr_s(y int32) VC {
	r := mkC()
	for i := range r {
		r[i] = x[i] >> y
	}
	return r
}
func (x VC) Shr_u(y int32) VC {
	r := mkC()
	for i := range r {
		r[i] = int8(uint8(x[i])) >> y
	}
	return r
}
func (x VC) Min_s(y VC) VC {
	r := mkC()
	for i := range r {
		r[i] = mini8(x[i], y[i])
	}
	return r
}
func (x VC) Max_s(y VC) VC {
	r := mkC()
	for i := range r {
		r[i] = maxi8(x[i], y[i])
	}
	return r
}
func (x VC) And(y VC) VC {
	r := mkC()
	for i := range r {
		r[i] = x[i] & y[i]
	}
	return r
}
func (x VC) Not() VC {
	r := mkC()
	for i := range r {
		r[i] = ^x[i]
	}
	return r
}

func (v VI) Neg() VI {
	r := mkI()
	for i := range r {
		r[i] = -v[i]
	}
	return r
}
func (v VI) Abs() VI {
	r := mkI()
	for i := range r {
		r[i] = absi32(v[i])
	}
	return r
}
func (x VI) Add(y VI) VI {
	r := mkI()
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x VI) Sub(y VI) VI {
	r := mkI()
	for i := range r {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x VI) Mul(y VI) VI {
	r := mkI()
	for i := range r {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x VI) And(y VI) VI {
	r := mkI()
	for i := range r {
		r[i] = x[i] & y[i]
	}
	return r
}
func (x VI) Shl(y int32) VI {
	r := mkI()
	for i := range r {
		r[i] = x[i] << y
	}
	return r
}
func (x VI) Shr_s(y int32) VI {
	r := mkI()
	for i := range r {
		r[i] = x[i] >> y
	}
	return r
}
func (x VI) Shr_u(y int32) VI {
	r := mkI()
	for i := range r {
		r[i] = int32(uint32(x[i])) >> y
	}
	return r
}
func (x VI) Min_s(y VI) VI {
	r := mkI()
	for i := range r {
		r[i] = mini32(x[i], y[i])
	}
	return r
}
func (x VI) Max_s(y VI) VI {
	r := mkI()
	for i := range r {
		r[i] = maxi32(x[i], y[i])
	}
	return r
}
func (x VI) Hmin_s() int32 {
	r := x[0]
	for i := 1; i < len(x); i++ {
		r = mini32(r, x[i])
	}
	return r
}
func (x VI) Hmax_s() int32 {
	r := x[0]
	for i := 1; i < len(x); i++ {
		r = maxi32(r, x[i])
	}
	return r
}
func (x VI) Hsum() int32 {
	r := x[0]
	for i := 1; i < len(x); i++ {
		r += x[i]
	}
	return r
}

func (v VF) Sqrt() VF {
	r := mkF()
	for i := range r {
		r[i] = math.Sqrt(v[i])
	}
	return r
}
func (v VF) Abs() VF {
	r := mkF()
	for i := range r {
		r[i] = math.Abs(v[i])
	}
	return r
}
func (v VF) Neg() VF {
	r := mkF()
	for i := range r {
		r[i] = -v[i]
	}
	return r
}
func (x VF) Add(y VF) VF {
	r := mkF()
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x VF) Sub(y VF) VF {
	r := mkF()
	for i := range r {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x VF) Mul(y VF) VF {
	r := mkF()
	for i := range r {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x VF) Div(y VF) VF {
	r := mkF()
	for i := range r {
		r[i] = x[i] / y[i]
	}
	return r
}
func (x VF) Pmin(y VF) VF {
	r := mkF()
	for i := range r {
		r[i] = pmin(x[i], y[i])
	}
	return r
}
func (x VF) Pmax(y VF) VF {
	r := mkF()
	for i := range r {
		r[i] = pmax(x[i], y[i])
	}
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

func (x VC) Eq(y VC) VC {
	r := mkC()
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VC) Ne(y VC) VC {
	r := mkC()
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VC) Lt_u(y VC) VC {
	r := mkC()
	for i := range r {
		if uint32(x[i]) < uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}
func (x VC) Lt_s(y VC) VC {
	r := mkC()
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VC) Gt_u(y VC) VC {
	r := mkC()
	for i := range r {
		if uint32(x[i]) > uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}
func (x VC) Gt_s(y VC) VC {
	r := mkC()
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}

func (x VI) Eq(y VI) VI {
	r := mkI()
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VI) Ne(y VI) VI {
	r := mkI()
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VI) Lt_s(y VI) VI {
	r := mkI()
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VI) Lt_u(y VI) VI {
	r := mkI()
	for i := range r {
		if uint32(x[i]) < uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}
func (x VI) Gt_s(y VI) VI {
	r := mkI()
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VI) Gt_u(y VI) VI {
	r := mkI()
	for i := range r {
		if uint32(x[i]) > uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}

func (x VF) Eq(y VF) VF {
	r := mkF()
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VF) Ne(y VF) VF {
	r := mkF()
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VF) Lt(y VF) VF {
	r := mkF()
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VF) Gt(y VF) VF {
	r := mkF()
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x VF) Hmin() float64 {
	r := x[0]
	for i := 1; i < len(x); i++ {
		r = pmin(r, x[i])
	}
	return r
}
func (x VF) Hmax() float64 {
	r := x[0]
	for i := 1; i < len(x); i++ {
		r = pmax(r, x[i])
	}
	return r
}
func (x VF) Hsum() float64 {
	r := x[0]
	for i := 1; i < len(x); i++ {
		r += x[i]
	}
	return r
}

/*
func (v F4) Replace_lane1(f float64) F4 {
	v[1] = f
	return v
}
func (v F4) Shuffle() (r F4) { r[0], r[1] = v[1], v[0]; return r }
func (v C4) All_true() int32 {
	for i := range v {
		if (v[i] != 0) == false {
			return 0
		}
	}
	return 1
}
func (v C4) Any_true() int32 {
	for i := range v {
		if v[i] != 0 {
			return 1
		}
	}
	return 0
}
func (v I4) Any_true() int32 {
	for i := range v {
		if v[i] != 0 {
			return 1
		}
	}
	return 0
}
func (v C4) Extract_lane_s0() int32 { return int32(v[0]) }
*/
