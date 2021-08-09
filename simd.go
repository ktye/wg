package wg

import (
	"fmt"
	"io"
	"strings"
)

var simdops map[string]string

func init() {
	simdops = make(map[string]string)
	for _, op := range []string{"Sqrt", "Neg", "Abs", "Add", "Sub", "Mul", "Div", "Shl", "Shr_u", "Shr_s", "Min_s", "Max_s", "Pmin", "Pmax", "Eq", "Ne", "Lt_u", "Lt_s", "Gt_u", "Gt_s", "All_true", "Any_true", "And", "Or", "Xor", "Not"} {
		for _, t := range []string{"I8x16", "I32x4", "F64x2"} {
			if t == "F64x2" && strings.HasSuffix(op, "_u") { // F64x2.Lt_u => F64x2.Lt
				op = strings.TrimSuffix(op, "_u")
			}
			s := t + "." + op
			simdops[s] = strings.ToLower(s)
			switch op {
			case "Any_true", "And", "Or", "Xor", "Not":
				simdops[s] = "v128." + strings.ToLower(op) // prefix is v128 not i8x16..
			}
		}
	}
}

func simd(x string, w io.Writer) bool {
	if op, ok := simdops[x]; ok {
		w.Write([]byte(op + "\n"))
		return true
	}
	var op string
	switch x {
	case "I8x16splat", "I32x4splat", "F64x2splat":
		op = fmt.Sprintf("%c%s.splat", x[0]+32, x[1:5])
	case "I8x16load", "I32x4load", "F64x2load":
		op = "v128.load"
	case "I8x16store", "I32x4store", "F64x2store":
		op = "v128.store"
	case "F64x2.Replace_lane1":
		op = "f64x2.replace_lane 1"
	case "I8x16.Extract_lane_s0":
		op = "i8x16.extract_lane_s 0"
	default:
		return false
	}
	w.Write([]byte(op + "\n"))
	return true
}
