//go:build ignore
package wg

// int32_t f(){
//  int32_t _0;
//  _0=0;
//  return _0;
// }
func f() int32 { return 0 }

// int32_t t(int32_t x){
//  int32_t _0;
//  _0=1;
//  return _0;
// }
func t(x int32) int32 { return 1 }

// uint32_t b(){
//  uint32_t x,_0;
//  x=(uint32_t)0;
//  _0=x;
//  return _0;
// }
func b() bool {
	var x bool
	return x
}

// int64_t zero(){
//  int64_t r,_0,_1,_2;
//  r=(int64_t)0;
//  _0=r;
//  _1=0;
//  _2=_0+_1;
//  return _2;
// }
func zero() (r int64) {
	return r + 0
}

// int32_t multiassign(int32_t x){
//  int32_t a,b,c,_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10;
//  a=(int32_t)0;
//  b=(int32_t)0;
//  c=(int32_t)0;
//  _0=x;
//  _1=t(_0);
//  _2=x;
//  _3=t(_2);
//  _4=x;
//  _5=t(_4);
//  a =_1;
//  b =_3;
//  c =_5;
//  _6=a;
//  _7=b;
//  _8=_6+_7;
//  _9=c;
//  _10=_8+_9;
//  return _10;
// }
func multiassign(x int32) int32 {
	a, b, c := t(x), t(x), t(x)
	return a + b + c
}
