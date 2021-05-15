//+build ignore

package x

// (func $Add (param $x i32) (param $y i32) (result i32)
// local.get $x local.get $y i32.add local.set $r local.get $r return)
func Add(x, y int32) int32 {
	r := x + y
	return r
}

// (func $rel (param $x i32) (param $y i32) (result u32)
// local.get $x i32.const 0 i32.gt_s local.get $y
// i32.const 0 i32.lt_s i32.and return)
func rel(x, y int32) bool { return x > 0 && y < 0 }

// custom types
type I int64

// (func $negi (param $x i64) (result i64)
// i64.const 0 local.get $x i64.sub return)
func negi(x I) I { return -x }

// (func $negf (param $x f32) (result f32)
// local.get $x f32.neg return)
func negf(x float32) float32 { return -x }

// structs
type st struct {
	a I
	b float64
}

// (func $structfn (param $s.a i64) (param $s.b f64)
// (param $y i64) (param $z f64) (result i64) (result f64)
// local.get $s.a local.get $y i64.add local.set $s.a
// local.get $s.b local.get $z f64.mul local.set $s.b
// local.get $s.a local.get $s.b return)
func structfn(s st, y int64, z float64) st {
	s.a += I(y)
	s.b *= z
	return s
}

// (func $st.method (param $s.a i64) (param $s.b f64) (param $y i64) (result i64)
// local.get $s.a local.get $y i64.div_s return)
func (s st) method(y int64) int64 { return int64(s.a) / y }

// (func $call (param $x f32) (result f32)
// local.get $x call $negf return)
func call(x float32) float32 { return negf(x) }

// embed
type st1 struct {
	a int32
}
type st2 struct {
	st1
	a int64
}
type st3 struct {
	b st1
	a int64
}

// (func $st3.m (param $s.b.a i32) (param $s.a i64))
func (s st3) m() {}

// (func $st1.method1 (param $s.a i32) (result i32)
// i32.const 2 local.get $s.a i32.mul return)
func (s st1) method1() int32 { return 2 * s.a }

// (func $callmethod1 (param $s.a i32) (result i32)
// local.get $s.a call $st1.method1 return)
func callmethod1(s st1) int32 { return s.method1() }

// (func $callinner1 (param $s.st1.a i32) (param $s.a i64) (result i32)
// local.get $s.st1.a call $st1.method1 return)
func callinner1(s st2) int32 { return s.st1.method1() }

/* not supported: direct method call of an embedded field
// (func $callinner2 (param $s.st1.a i32) (param $s.a i64) (result i32)
// local.get $s.st1.a call $st1.method1 return)
func callinner2(s st2) int32 { return s.method1() }
*/
