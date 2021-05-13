//+build ignore

package x

type I int32

type st struct {
	a I
	b float64
}

// func Add(x I, y st) (r st) { r.a = x + y.a; return r }
func Add(x, y int32) int32 {
	r := x + y
	return r
}
