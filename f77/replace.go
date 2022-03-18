package f77

var replace map[string]string

func init() {
	r := make(map[string]string)
	r["mtC"] = MTC
	replace = r
}

// ? is replaced with name77
// #1..#8 are replaced by memsize()

const MTC = `SUBROUTINE ?(X,Y,V,E,R)
INTEGER*1 I8(#1)
INTEGER*4 X,Y,V,E,R
COMMON /MEM/I8
R=0*V
write(*,*) 1+X,X+E-Y,1+Y,E
IF(ALL(I8(1+X:X+E-Y).EQ.I8(1+Y:E)))THEN
R=1
ENDIF
RETURN
END
`
