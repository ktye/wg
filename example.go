//+build ignore

package example

import _ "github.com/ktye/wg/module"

func add(x int32, y int32) int32              { return x + y }
func sub(x int32, y int32) int32              { return x - y }
func multivalue(x, y int32) (sum, diff int32) { return x + y, x - y }

func sum(addr int32, n int32) float64 {
	var r float64
	for n > 0 {
		r += GetF64(addr)
		addr += 8
		n--
	}
	return r
}

func sum2(x int32, n int32) float64 {
	r := 0.0
	for _, f := range F64[x : x+n] {
		r += f
	}
}

func addf(x, y float64) float64 { return x + y }

type f2 = func(float64, float64) float64

// reduce uses an indirect function call, e.g. the addf function.
func reduce(f, x, n int32) float64 {
	r := F64[x]
	e := x + n
	for _, v := range F64[1+x : n+x] {
		r = F[f].(f2)(r, v)
	}
	return r
}

func init() {
	Memory(1)
	Export(add, sub, sum)
	Functions(0, addf)
}
