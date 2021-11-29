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
//  uint32_t x,_0,_1;
//  _0=(uint32_t)0;
//  x =_0;
//  _1=x;
//  return _1;
// }
func b() bool {
	var x bool
	return x
}

// int32_t multiassign(int32_t x){
//  int32_t a,b,c,_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10;
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
