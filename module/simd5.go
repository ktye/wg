package module

// gcc 32byte vector extensions(avx2..)
type C5 [32]int8
type I5 [8]int32
type F5 [4]float64

func Iota5() I5 { return I5{0, 1, 2, 3, 4, 5, 6, 7} }
func C5load(addr int32) (r C4) {
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
	SetF64(32+addr, v[3])
}

func C5splat(x int32) (r C5) {
	for i := range r {
		r[i] = int8(x)
	}
	return r
}
func I5splat(x int32) (r I5)   { return I5{x, x, x, x, x, x, x, x} }
func F5splat(x float64) (r F5) { return F5{x, x, x, x} }

func (x I5) Add(y I5) (r I5) {
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}

/*
func (v F4) Replace_lane1(f float64) F4 {
	v[1] = f
	return v
}
func (v F4) Shuffle() (r F4) { r[0], r[1] = v[1], v[0]; return r }

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

func (v C4) Neg() (r C4) {
	for i := range r {
		r[i] = -v[i]
	}
	return r
}
func (v C4) Abs() (r C4) {
	for i := range r {
		r[i] = absi8(v[i])
	}
	return r
}
func (x C4) Add(y C4) (r C4) {
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x C4) Sub(y C4) (r C4) {
	for i := range r {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x C4) Mul(y C4) (r C4) {
	for i := range r {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x C4) Shl(y int32) (r C4) {
	for i := range r {
		r[i] = x[i] << y
	}
	return r
}
func (x C4) Shr_s(y int32) (r C4) {
	for i := range r {
		r[i] = x[i] >> y
	}
	return r
}
func (x C4) Shr_u(y int32) (r C4) {
	for i := range r {
		r[i] = int8(uint8(x[i])) >> y
	}
	return r
}
func (x C4) Min_s(y C4) (r C4) {
	for i := range r {
		r[i] = mini8(x[i], y[i])
	}
	return r
}
func (x C4) Max_s(y C4) (r C4) {
	for i := range r {
		r[i] = maxi8(x[i], y[i])
	}
	return r
}
func (x C4) And(y C4) (r C4) {
	for i := range r {
		r[i] = x[i] & y[i]
	}
	return r
}
func (x C4) Not() (r C4) {
	for i := range r {
		r[i] = ^x[i]
	}
	return r
}

func (v I4) Neg() (r I4) {
	for i := range r {
		r[i] = -v[i]
	}
	return r
}
func (v I4) Abs() (r I4) {
	for i := range r {
		r[i] = absi32(v[i])
	}
	return r
}
func (x I4) Add(y I4) (r I4) {
	for i := range r {
		r[i] = x[i] + y[i]
	}
	return r
}
func (x I4) Sub(y I4) (r I4) {
	for i := range r {
		r[i] = x[i] - y[i]
	}
	return r
}
func (x I4) Mul(y I4) (r I4) {
	for i := range r {
		r[i] = x[i] * y[i]
	}
	return r
}
func (x I4) Shl(y int32) (r I4) {
	for i := range r {
		r[i] = x[i] << y
	}
	return r
}
func (x I4) Shr_s(y int32) (r I4) {
	for i := range r {
		r[i] = x[i] >> y
	}
	return r
}
func (x I4) Shr_u(y int32) (r I4) {
	for i := range r {
		r[i] = int32(uint32(x[i])) >> y
	}
	return r
}
func (x I4) Min_s(y I4) (r I4) {
	for i := range r {
		r[i] = mini32(x[i], y[i])
	}
	return r
}
func (x I4) Max_s(y I4) (r I4) {
	for i := range r {
		r[i] = maxi32(x[i], y[i])
	}
	return r
}

func (v F4) Sqrt() F4 { return F4{math.Sqrt(v[0]), math.Sqrt(v[1])} }
func (v F4) Abs() F4  { return F4{math.Abs(v[0]), math.Abs(v[1])} }
func (v F4) Neg() F4  { return F4{-v[0], -v[1]} }
func (x F4) Add(y F4) (r F4) {
	r[0] = x[0] + y[0]
	r[1] = x[1] + y[1]
	return r
}
func (x F4) Sub(y F4) (r F4) {
	r[0] = x[0] - y[0]
	r[1] = x[1] - y[1]
	return r
}
func (x F4) Mul(y F4) (r F4) {
	r[0] = x[0] * y[0]
	r[1] = x[1] * y[1]
	return r
}
func (x F4) Div(y F4) (r F4) {
	r[0] = x[0] / y[0]
	r[1] = x[1] / y[1]
	return r
}
func (x F4) Pmin(y F4) (r F4) {
	r[0] = pmin(x[0], y[0])
	r[1] = pmin(x[1], y[1])
	return r
}
func (x F4) Pmax(y F4) (r F4) {
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

func (x C4) Eq(y C4) (r C4) {
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x C4) Ne(y C4) (r C4) {
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x C4) Lt_u(y C4) (r C4) {
	for i := range r {
		if uint32(x[i]) < uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}
func (x C4) Lt_s(y C4) (r C4) {
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x C4) Gt_u(y C4) (r C4) {
	for i := range r {
		if uint32(x[i]) > uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}
func (x C4) Gt_s(y C4) (r C4) {
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}

func (x I4) Eq(y I4) (r I4) {
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I4) Ne(y I4) (r I4) {
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I4) Lt_s(y I4) (r I4) {
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I4) Lt_u(y I4) (r I4) {
	for i := range r {
		if uint32(x[i]) < uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}
func (x I4) Gt_s(y I4) (r I4) {
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x I4) Gt_u(y I4) (r I4) {
	for i := range r {
		if uint32(x[i]) > uint32(y[i]) {
			r[i] = -1
		}
	}
	return r
}

func (x F4) Eq(y F4) (r F4) {
	for i := range r {
		if x[i] == y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x F4) Ne(y F4) (r F4) {
	for i := range r {
		if x[i] != y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x F4) Lt_s(y F4) (r F4) {
	for i := range r {
		if x[i] < y[i] {
			r[i] = -1
		}
	}
	return r
}
func (x F4) Gt_s(y F4) (r F4) {
	for i := range r {
		if x[i] > y[i] {
			r[i] = -1
		}
	}
	return r
}
*/
