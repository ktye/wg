package f77

var replace map[string]string

var repmat map[string]string

func init() {
	r := make(map[string]string)
	r["mtC"] = MTC
	r["all"] = ALL
	r["any"] = ANY
	r["inC"] = INC
	r["inI"] = INI
	r["not"] = NOT
	r["cosin_"] = COSIN
	r["atan"] = ""
	r["satan"] = ""
	r["xatan"] = ""
	r["expmulti"] = ""
	r["ldexp"] = ""
	r["frexp"] = ""
	r["normalize"] = ""
	r["modabsf"] = ""
	r["pow"] = ""
	r["iipow"] = ""
	r["ipow"] = ""
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
