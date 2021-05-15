package example

import . "github.com/ktye/wg/module"

func Add(x int32, y int32) int32              { return x + y }
func Sub(x int32, y int32) int32              { return x - y }
func multivalue(x, y int32) (sum, diff int32) { return x + y, x - y }

func sum(x int32, n int32) (r float64) {
	for i := int32(0); i < n; i++ {
		r += F64(x)
		x += 8
	}
	return r
}

func addf(x, y float64) float64 { return x + y }

type f2 = func(float64, float64) float64

// reduce uses an indirect function call, e.g. the addf function.
func reduce(f, x, n int32) float64 {
	r := F64(x)
	for i := int32(1); i < n; i++ {
		x += 8
		r = Func[f].(f2)(r, F64(x))
	}
	return r
}

func init() {
	Memory(1)
	Functions(0, addf)
}
