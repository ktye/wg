package wg

import (
	"fmt"
	"io"
	"strings"
)

func simd(x string, w io.Writer) bool {
	var op string
	switch x {
	case "I8x16splat", "I16x8splat", "I32x4splat", "I64x2splat", "F32x4splat", "F64x2splat":
		op = fmt.Sprintf("%c%s.splat", x[0]+32, x[1:5])
	case "I8x16load", "I16x8load", "I32x4load", "I64x2load", "F32x4load", "F64x2load":
		op = "v128.load"
	case "I8x16store", "I16x8store", "I32x4store", "I64x2store", "F32x4store", "F64x2store":
		op = "v128.store"
	case "I8x16all_true":
		op = "i8x16.all_true"
	case "I8x16any_true":
		op = "i8x16.any_true"
	case "F64x2replace_lane1":
		op = "f64x2.replace_lane 1"
	default:
		for _, op := range []string{"sqrt", "neg", "abs", "add", "sub", "mul", "div", "min_s", "max_s", "min", "max", "eq", "ne", "lt", "lt_s", "gt", "gt_s"} {
			if strings.HasPrefix(x, "I8x16") || strings.HasPrefix(x, "I32x4") || strings.HasPrefix(x, "I64x2") || strings.HasPrefix(x, "F32x4") || strings.HasPrefix(x, "F64x2") {
				if strings.HasSuffix(x, op) {
					s := fmt.Sprintf("%c%s.%s\n", x[0]+32, x[1:5], op) // I8x16add -> i8x16.add(a: v128, b: v128)
					w.Write([]byte(s + "\n"))
					return true
				}
			}
		}
		return false
	}
	w.Write([]byte(op + "\n"))
	return true
}
