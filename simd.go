package wg

import (
	"io"
	"strings"
)

var simdops map[string]string

func init() {
	m := map[string]string{"VC": "i8x16", "VI": "i32x4", "VF": "f64x2"}
	simdops = make(map[string]string)
	for _, op := range []string{"Sqrt", "Neg", "Abs", "Add", "Sub", "Mul", "Div", "Shl", "Shr_u", "Shr_s", "Min_s", "Max_s", "Pmin", "Pmax", "Eq", "Ne", "Lt_u", "Lt_s", "Gt_u", "Gt_s", "All_true", "Any_true", "And", "Or", "Xor", "Not"} {
		for _, t := range []string{"VC", "VI", "VF"} {
			//if t == "VF" && strings.HasSuffix(op, "_u") { // F64x2.Lt_u => F64x2.Lt
			//	op = strings.TrimSuffix(op, "_u")
			//}
			s := t + "." + op
			simdops[s] = strings.ToLower(m[t] + "." + op)
			switch op {
			case "Any_true", "And", "Or", "Xor", "Not":
				simdops[s] = "v128." + strings.ToLower(op) // prefix is v128 not i8x16..
			}
		}
	}
	for k, v := range map[string]string{
		"VCsplat": "i8x16.splat", "VIsplat": "i32x4.splat", "VFsplat": "f64x2.splat",
		"VCload": "v128.load", "VIload": "v128.load", "VFload": "v128.load",
		"VCstore": "v128.store", "VIstore": "v128.store", "VFstore": "v128.store",
		"Iota": "v128.const i32x4 0 1 2 3", "VI1": "v128.const i32x4 1 1 1 1",
		"VIloadB":   "i32.load\ni32x4.splat\ni16x8.extend_low_i8x16_s\ni32x4.extend_low_i16x8_s",
		"VI.Hmin_s": "i32x4.extract_lane 0\n!i32x4.extract_lane 1\ncall $mini\n!i32x4.extract_lane 2\ncall $mini\n!i32x4.extract_lane 3\ncall $mini",
		"VI.Hmax_s": "i32x4.extract_lane 0\n!i32x4.extract_lane 1\ncall $maxi\n!i32x4.extract_lane 2\ncall $maxi\n!i32x4.extract_lane 3\ncall $maxi",
		"VI.Hsum":   "i32x4.extract_lane 0\n!i32x4.extract_lane 1\ni32.add\n!i32x4.extract_lane 2\ni32.add\n!i32x4.extract_lane 3\ni32.add",
		"VF.Hmin":   "f64x2.extract_lane 0\n!f64x2.extract_lane 1\nf64.min",
		"VF.Hmax":   "f64x2.extract_lane 0\n!f64x2.extract_lane 1\nf64.max",
		"VF.Hsum":   "f64x2.extract_lane 0\n!f64x2.extract_lane 1\nf64.add",
	} {
		simdops[k] = v
	}
}

func simd(x string, args []Expr, w io.Writer) bool {
	r := func(s string) string {
		var a strings.Builder
		args[0].wat(&a)
		return strings.ReplaceAll(s, "!", a.String())
	}
	if op, ok := simdops[x]; ok {
		if strings.HasPrefix(x, "VI.H") || strings.HasPrefix(x, "VF.H") { // VI.Hmin_s|Hmax_s|Hsum duplicate argument
			op = r(op)
		}
		w.Write([]byte(op + "\n"))
		return true
	}
	return false
}
