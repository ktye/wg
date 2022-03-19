package f77

import "strings"

var replace map[string]string

var repmat map[string]string

func init() {
	do := func(x, y string) string { return strings.ReplaceAll(x, "!", y) }
	r := make(map[string]string)
	r["mtC"] = MTC
	r["all"] = ALL
	r["any"] = ANY
	r["inC"] = INC
	r["inI"] = INI
	r["not"] = NOT
	r["cosin_"] = COSIN
	r["negC"] = do(F1C, "-")
	r["negI"] = do(F1I, "-")
	r["negF"] = do(F1F, "-")
	r["absC"] = do(F1C, "ABS")
	r["absI"] = do(F1I, "ABS")
	r["absF"] = do(F1F, "ABS")
	r["sqrF"] = SQRF

	v := strings.Split("add+ sub- mul* div/", " ")
	for _, s := range v {
		op := s[3:]
		s = s[:3]
		r[s+"cC"] = do(F2SC, op) // atom-vector
		r[s+"iI"] = do(F2SI, op)
		r[s+"fF"] = do(F2SF, op)
		r[s+"zZ"] = do(F2SZ, op)
		r[s+"C"] = do(F2C, op) // vector-vector
		r[s+"I"] = do(F2I, op)
		r[s+"F"] = do(F2F, op)
		r[s+"Z"] = do(F2Z, op)
	}

	r["divIi"] = DIVIIS

	prefix := func(x string, flip bool) string { // r=x!y => r=!(x,y)
		split := func(s, p string) (string, string) { v := strings.Split(s, p); return v[0], v[1] }
		v := strings.Split(x, "\n")
		for i, s := range v {
			if strings.Index(s, "!") > 0 {
				a, b := split(s, " = ")
				d, e := split(b, " ! ")
				if flip {
					d, e = e, d
				}
				v[i] = a + " = !(" + d + ", " + e + ")"
			}
		}
		return strings.Join(v, "\n")
	}
	for _, s := range []string{"min", "max"} {
		q := strings.ToUpper(s)
		r[s+"cC"] = do(prefix(F2SC, true), q)
		r[s+"iI"] = do(prefix(F2SI, true), q)
		r[s+"fF"] = do(prefix(F2SF, true), q)
		r[s+"C"] = do(prefix(F2C, false), q)
		r[s+"I"] = do(prefix(F2I, false), q)
		r[s+"F"] = do(prefix(F2F, false), q)
	}

	// exclude softmath.
	m := "atan satan xatan expmulti ldexp frexp normalize modabsf pow iipow ipow absf"
	for _, s := range strings.Split(m, " ") {
		r[s] = ""
	}
	replace = r

	repmat = map[string]string{
		"hypot": "HYPOT",
		"atan2": "ATAN2",
		"exp":   "EXP",
		"log":   "LOG",
	}
}

// ? is replaced with name77
// #1..#8 are replaced by memsize()

const MTC = `INTEGER*4 FUNCTION ?(X,Y,V,E)
INTEGER*4 X,Y,V,E
INTEGER*1 I8(#1)
COMMON /MEM/I8
? = 0*V
IF(ALL(I8(1+X:X+E-Y).EQ.I8(1+Y:E))) ? = 1
RETURN
END
`
const ALL = `INTEGER*4 FUNCTION ?(X,N)
INTEGER*4 X,N
INTEGER*1 I8(#1)
COMMON /MEM/I8
? = 0
IF(ALL(I8(1+X:X+N).EQ.1)) ? = 1
RETURN
END
`
const ANY = `INTEGER*4 FUNCTION ?(X,N)
INTEGER*4 X,N
INTEGER*1 I8(#1)
COMMON /MEM/I8
? = 1
IF(ALL(I8(1+X:X+N).EQ.0)) ? = 0
RETURN
END
`
const INC = `INTEGER*4 FUNCTION ?(X,Y,V,E)
INTEGER*4 X,Y,V,E
INTEGER*1 I8(#1)
COMMON /MEM/I8
? = 0*V
IF(ANY(I8(1+Y:E).EQ.INT(X,1))) ? = 1
RETURN
END
`
const INI = `INTEGER*4 FUNCTION ?(X,Y,V,E)
INTEGER*4 X,Y,V,E
INTEGER*4 I32(#4)
INTEGER*1 I8(#1)
COMMON /MEM/I8
EQUIVALENCE(I8,I32)
? = 0*V
IF(ANY(I32(1+Y/4:E/4).EQ.X)) ? = 1
RETURN
END
`
const NOT = `SUBROUTINE ?(X,R,E)
INTEGER*4 X,E,R
INTEGER*1 I8(#1)
COMMON /MEM/I8
I8(1+R:E) = IAND(INT(1,1),NOT(I8(1+X:X+E-R)))
RETURN
END
`
const COSIN = `SUBROUTINE ?(X,C,S)
REAL*8 X,C,S
C = COS(X)
S = SIN(X)
RETURN
END
`
const F1C = `SUBROUTINE ?(X,R,E)
INTEGER*4 X,R,E
INTEGER*1 I8(#1)
COMMON /MEM/I8
I8(1+R:E) = !(I8(1+X:X+E-R))
RETURN
END
`
const F1I = `SUBROUTINE ?(X,R,E)
INTEGER*4 X,R,E
INTEGER*1 I8(#1)
INTEGER*4 I32(#4)
COMMON /MEM/I8
EQUIVALENCE(I8,I32)
I32(1+R/4:E/4) = !(I32(1+X/4:(X+E-R)/4))
RETURN
END
`
const F1F = `SUBROUTINE ?(X,R,E)
INTEGER*4 X,R,E
INTEGER*1 I8(#1)
REAL*8 F64(#8)
COMMON /MEM/I8
EQUIVALENCE(I8,F64)
F64(1+R/8:E/8) = !(F64(1+X/8:(X+E-R)/8))
RETURN
END
`

/*
const ABSC = `SUBROUTINE ?(X,R,E)
INTEGER*4 X,R,E
INTEGER*1 I8(#1)
COMMON /MEM/I8
I8(1+R:E) = ABS(I8(1+X:X+E-R))
RETURN
END
`
const ABSI = `SUBROUTINE ?(X,R,E)
INTEGER*4 X,R,E
INTEGER*1 I8(#1)
INTEGER*4 I32(#4)
COMMON /MEM/I8
EQUIVALENCE(I8,I32)
I32(1+R/4:E/4) = ABS(1+X/4:(X+E-R)/4)
RETURN
END
`
const ABSF = `SUBROUTINE ?(X,R,E)
INTEGER*4 X,R,E
INTEGER*1 I8(#1)
REAL*8 F64(#8)
COMMON /MEM/I8
EQUIVALENCE(I8,F64)
F64(1+R/8:E/8) = ABS(1+X/8:(X+E-R)/8)
RETURN
END
`
*/
const SQRF = `SUBROUTINE ?(X,R,E)
INTEGER*4 X,R,E
INTEGER*1 I8(#1)
REAL*8 F64(#8)
COMMON /MEM/I8
EQUIVALENCE(I8,F64)
F64(1+R/8:E/8) = SQRT(1+X/8:(X+E-R)/8)
RETURN
END
`
const F2SC = `SUBROUTINE ?(X,Y,R,E)
INTEGER*1 X
INTEGER*4 Y,R,E
INTEGER*1 I8(#1)
COMMON /MEM/I8
I8(1+R:E) = X ! I8(1+R:E)
RETURN
END
`
const F2SI = `SUBROUTINE ?(X,Y,R,E)
INTEGER*4 X,Y,R,E
INTEGER*1 I8(#1)
INTEGER*4 I32(#4)
COMMON /MEM/I8
EQUIVALENCE(I8,I32)
I32(1+R/4:E/4) = X ! I32(1+R/4:E/4)
RETURN
END
`
const F2SF = `SUBROUTINE ?(X,Y,R,E)
REAL*8    X
INTEGER*4 Y,R,E
INTEGER*1 I8(#1)
REAL*8    F64(#8)
COMMON /MEM/I8
EQUIVALENCE(I8,F64)
F64(1+R/8:E/8) = X ! F64(1+R/8:E/8)
RETURN
END
`
const F2SZ = `SUBROUTINE ?(RE,IM,Y,R,E)
REAL*8    RE,IM
INTEGER*4 Y,R,E
INTEGER*1 I8(#1)
COMPLEX*16 X
COMPLEX*16 Z(#z)
COMMON /MEM/I8
EQUIVALENCE(I8,Z)
X = CMPLX(RE,IM)
Z(1+R/16:E/16) = X ! Z(1+R/16:E/16)
RETURN
END
`
const F2C = `SUBROUTINE ?(X,Y,R,E)
INTEGER*4 X,Y,R,E
INTEGER*1 I8(#1)
COMMON /MEM/I8
I8(1+R:E) = I8(1+X:X+E-R) ! I8(1+Y:Y+E-R)
RETURN
END
`
const F2I = `SUBROUTINE ?(X,Y,R,E)
INTEGER*4 X,Y,R,E
INTEGER*1 I8(#1)
INTEGER*4 I32(#4)
COMMON /MEM/I8
EQUIVALENCE(I8,I32)
I32(1+R/4:E/4) = I32(1+X/4:(X+E-R)/4) ! I32(1+Y/4:(Y+E-R)/4)
RETURN
END
`
const F2F = `SUBROUTINE ?(X,Y,R,E)
INTEGER*4 X,Y,R,E
INTEGER*1 I8
REAL*8    F64
COMMON /MEM/I8
EQUIVALENCE(I8,F64)
F64(1+R/8:E/8) = F64(1+X/8:(X+E-R)/8) ! F64(1+Y/8:(Y+E-R)/8)
RETURN
END
`
const F2Z = `SUBROUTINE ?(X,Y,R,E)
INTEGER*4 X,Y,R,E
INTEGER*1 I8
COMPLEX*16 Z
COMMON /MEM/I8
EQUIVALENCE(I8,Z)
Z(1+R/16:E/16) = Z(1+X/16:(X+E-R)/16) ! Z(1+Y/16:(Y+E-R)/16)
RETURN
END
`
const DIVIIS = `SUBROUTINE ?(X,Y,E)
INTEGER*4 X,Y,E
INTEGER*1 I8(#1)
INTEGER*4 I32(#4)
COMMON /MEM/I8
EQUIVALENCE(I8,I32)
I32(1+X/4:E/4) = I32(1+X/4:E/4) / Y
RETURN
END
`
