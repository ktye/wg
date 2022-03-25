package f77

import (
	"strings"
)

var replace map[string]string

var repmat map[string]string

func init() {
	// @ #n ? is replaced in fn() + RETURN END
	do := func(x, y string) string {
		m := make(map[string]bool)
		for _, s := range []string{"cc", "ii", "ff", "zz"} {
			if strings.Index(x, s) >= 0 {
				m["cc"] = true
				m[s] = true
			}
		}
		h, e := "", ""
		if m["cc"] {
			h += "INTEGER*1 I8(#1)\n"
		}
		if m["ii"] {
			h += "INTEGER*4 I32(#4)\n"
			e += ",I32"
		}
		if m["ff"] {
			h += "REAL*8 F64(#8)\n"
			e += ",F64"
		}
		if m["zz"] {
			h += "COMPLEX*16 ZZ(#z)\n"
			e += ",ZZ"
		}
		if h != "" {
			h += "COMMON /MEM/I8\n"
		}
		if e != "" {
			h += "EQUIVALENCE(I8" + e + ")\n"
		}
		x = strings.ReplaceAll(x, "cc", "I8")
		x = strings.ReplaceAll(x, "ii", "I32")
		x = strings.ReplaceAll(x, "ff", "F64")
		x = strings.ReplaceAll(x, "zz", "ZZ")
		x = strings.ReplaceAll(x, "c", "INTEGER*1")
		x = strings.ReplaceAll(x, "i", "INTEGER*4")
		x = strings.ReplaceAll(x, "f", "REAL*8")
		x = strings.ReplaceAll(x, "z", "COMPLEX*16")
		x = strings.ReplaceAll(x, "!", y)
		h = strings.TrimSuffix(h, "\n")
		if h == "" && strings.Index(x, "@") >= 0 {
			panic("no header but anchor: " + x)
		}
		if h != "" && strings.Index(x, "@") < 0 {
			panic("no anchor but header: " + x)
		}
		return strings.Replace(x, "@", "\n"+h, 1)
	}
	r := make(map[string]string)
	r["mtC"] = do(MTC, "")
	r["all"] = do(ALL, "")
	r["any"] = do(ANY, "")
	r["inC"] = do(INC, "")
	r["inI"] = do(INI, "")
	r["not"] = do(NOT, "")
	r["cosin_"] = do(COSIN, "")
	r["seqi"] = do(SEQI, "")
	r["fwh"] = do(FWH, "") // *&B
	r["store"] = do(STORE, "")
	r["catch"] = do(CATCH, "")

	// monadic
	r["negC"] = do(F1C, "-")
	r["negI"] = do(F1I, "-")
	r["negF"] = do(F1F, "-")
	r["absC"] = do(F1C, "ABS")
	r["absI"] = do(F1I, "ABS")
	r["absF"] = do(F1F, "ABS")
	r["sqrF"] = do(SQRF, "")

	// dyadic
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

	r["divIi"] = do(DIVIIS, "")

	// minmax
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

	// compare
	for _, s := range []string{"eq", "lt", "gt"} {
		q := "." + strings.ToUpper(s) + "."
		r[s+"cC"] = do(CAVC, q)
		r[s+"iI"] = do(CAVI, q)
		r[s+"fF"] = do(CAVF, q)
		r[s+"Cc"] = do(CVAC, q)
		r[s+"Ii"] = do(CVAI, q)
		r[s+"Ff"] = do(CVAF, q)
		r[s+"C"] = do(CVC, q)
		r[s+"I"] = do(CVI, q)
		r[s+"F"] = do(CVF, q)
	}

	// exclude softmath.
	m := "isnan atan satan xatan expmulti ldexp frexp normalize modabsf pow iipow ipow"
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

const MTC = `i FUNCTION ?(X,Y,V,E)
i X,Y,V,E@
? = 0*V
IF(ALL(cc(1+X:X+E-Y).EQ.cc(1+Y:E))) ? = 1`
const ALL = `i FUNCTION ?(X,N)
i X,N@
? = 0
IF(ALL(cc(1+X:X+N).EQ.1)) ? = 1`
const ANY = `i FUNCTION ?(X,N)
i X,N@
? = 1
IF(ALL(cc(1+X:X+N).EQ.0)) ? = 0`
const INC = `i FUNCTION ?(X,Y,V,E)
i X,Y,V,E@
? = 0*V
IF(ANY(cc(1+Y:E).EQ.INT(X,1))) ? = 1`
const INI = `i FUNCTION ?(X,Y,V,E)
i X,Y,V,E@
? = 0*V
IF(ANY(ii(1+Y/4:E/4).EQ.X)) ? = 1`
const NOT = `SUBROUTINE ?(X,R,E)
i X,E,R@
cc(1+R:E) = IAND(INT(1,1),NOT(cc(1+X:X+E-R)))`
const COSIN = `SUBROUTINE ?(X,C,S)
f X,C,S
C = COS(X)
S = SIN(X)`
const SEQI = `SUBROUTINE ?(P,E)
i P,E,R@
R = P/4
DO I=1,(E-P)/4,4
 ii(R+I) = I-1
 ii(R+I+1) = I
 ii(R+I+2) = I+1
 ii(R+I+3) = I+2
ENDDO`
const FWH = `SUBROUTINE ?(X,N,R)
i X,N,R@
R = FINDLOC(cc(1+X:X+N),INT(1,1),1)
IF(R.EQ.0) R = -2147483647
R = R - 1`
const STORE = `SUBROUTINE ?()` //nyi
const CATCH = `SUBROUTINE ?()`
const F1C = `SUBROUTINE ?(X,R,E)
i X,R,E@
cc(1+R:E) = !(cc(1+X:X+E-R))`
const F1I = `SUBROUTINE ?(X,R,E)
i X,R,E@
ii(1+R/4:E/4) = !(ii(1+X/4:(X+E-R)/4))`
const F1F = `SUBROUTINE ?(X,R,E)
i X,R,E@
ff(1+R/8:E/8) = !(ff(1+X/8:(X+E-R)/8))`
const SQRF = `SUBROUTINE ?(X,R,E)
i X,R,E@
ff(1+R/8:E/8) = SQRT(ff(1+X/8:(X+E-R)/8))`
const F2SC = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
cc(1+R:E) = INT(X,1) ! cc(1+R:E)`
const F2SI = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
ii(1+R/4:E/4) = X ! ii(1+R/4:E/4)`
const F2SF = `SUBROUTINE ?(X,Y,R,E)
f X
i Y,R,E@
ff(1+R/8:E/8) = X ! ff(1+R/8:E/8)`
const F2SZ = `SUBROUTINE ?(RE,IM,Y,R,E)
f RE,IM
i Y,R,E
z X@
X = COMPLEX(RE,IM)
zz(1+R/16:E/16) = X ! zz(1+R/16:E/16)`
const F2C = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
cc(1+R:E) = cc(1+X:X+E-R) ! cc(1+Y:Y+E-R)`
const F2I = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
ii(1+R/4:E/4) = ii(1+X/4:(X+E-R)/4) ! ii(1+Y/4:(Y+E-R)/4)`
const F2F = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
ff(1+R/8:E/8) = ff(1+X/8:(X+E-R)/8) ! ff(1+Y/8:(Y+E-R)/8)`
const F2Z = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
zz(1+R/16:E/16) = zz(1+X/16:(X+E-R)/16) ! zz(1+Y/16:(Y+E-R)/16)`
const DIVIIS = `SUBROUTINE ?(X,Y,E)
i X,Y,E@
ii(1+X/4:E/4) = ii(1+X/4:E/4) / Y`

// todo args are flipped
const CAVC = `SUBROUTINE ?(X,I,Y,R,E)
i X,I,Y,R,E@
cc(1+R:E) = MERGE(INT(1,1),INT(0,1),cc(1+Y:Y+E-R) ! INT(X,1))`
const CAVI = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
cc(1+R:E) = MERGE(INT(1,1),INT(0,1),ii(1+Y/4:(Y+E-R)/4) ! X)`
const CAVF = `SUBROUTINE ?(X,Y,R,E)
f X
i Y,R,E@
cc(1+R:E) = MERGE(INT(1,1),INT(0,1),ff(1+Y/8:(Y+E-R)/8) ! X)`

const CVAC = `SUBROUTINE ?(X,I,Y,R,E)
i X,I,Y,R,E@
cc(1+R:E) = MERGE(INT(1,1),INT(0,1),cc(1+X:X+E-R) ! INT(Y,1))`
const CVAI = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
cc(1+R:E) = MERGE(INT(1,1),INT(0,1),ii(1+X/4:(X+E-R)/4) ! Y)`
const CVAF = `SUBROUTINE ?(X,Y,R,E)
f Y
i X,R,E@
cc(1+R:E) = MERGE(INT(1,1),INT(0,1),ff(1+X/8:(X+E-R)/8) ! Y)`

const CVC = `SUBROUTINE ?(X,I,Y,R,E)
i X,I,Y,R,E@
cc(1+R:E) = MERGE(INT(1,1),INT(0,1),cc(1+X:X+E-R) ! cc(1+Y:Y+E-R))`
const CVI = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
cc(1+R:E) = MERGE(INT(1,1),INT(0,1),ii(1+X/4:(X+E-R)/4) ! ii(1+Y/4:(Y+E-R)/4))`
const CVF = `SUBROUTINE ?(X,Y,R,E)
i X,Y,R,E@
cc(1+R:E) = MERGE(INT(1,1),INT(0,1),ff(1+X/8:(X+E-R)/8) ! ff(1+Y/8:(X+E-R)/8))`
