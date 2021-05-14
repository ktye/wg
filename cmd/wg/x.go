//+build ignore

package x

// (func $Add (param $x i32) (param $y i32) (result i32)
// local.get $x local.get $y i32.add local.set $r local.get $r)
func Add(x, y int32) int32 {
	r := x + y
	return r
}

// (func $rel (param $x i32) (param $y i32) (result u32)
// local.get $x i32.const 0 i32.gt_s local.get $y
// i32.const 0 i32.lt_s i32.and)
func rel(x, y int32) bool { return x > 0 && y < 0 }

// custom types
type I int64

// (func $negi (param $x i64) (result i64)
// i64.const 0 local.get $x i64.sub)
func negi(x I) I { return -x }

// (func $negf (param $x f32) (result f32)
// local.get $x f32.neg)
func negf(x float32) float32 { return -x }

// structs
type st struct {
	a I
	b float64
}

// (func $structfn (param $s_a i64) (param $s_b f64)
// (param $y i64) (param $z f64) (result i64) (result f64)
// local.get $s_a local.get $y i64.add local.set $s_a
// local.get $s_b local.get $z f64.mul local.set $s_b
// local.get $s_a local.get $s_b)
func structfn(s st, y int64, z float64) st {
	s.a += I(y)
	s.b *= z
	return s
}

// (func $method (param $s_a i64) (param $s_b f64) (param $y i64) (result i64)
// local.get $s_a local.get $y i64.div_s)
func (s st) method(y int64) int64 { return int64(s.a) / y }
