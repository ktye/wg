package main

import (
	"os"
	"strings"

	"github.com/ktye/wg"
	"github.com/ktye/wg/f77"
)

func main() {
	a := os.Args[1:]
	f := false
	k := false
	a0 := func() string {
		if len(a) == 0 {
			return ""
		}
		return a[0]
	}
	if a0() == "-try" {
		wg.TryCatch, a = true, a[1:]
	}
	if a0() == "-multi" {
		wg.MultiMemory, a = true, a[1:]
	}
	if a0() == "-tags" {
		wg.BuildContext.BuildTags, a = strings.Split(a[1], ","), a[2:]
	}
	if a0() == "-nosys" { // wasm-only, no system interface (and no imports)
		wg.NoSys, a = true, a[1:]
	}
	if a0() == "-f" { // f77
		f, a = true, a[1:]
	}
	if a0() == "-k" {
		k, a = true, a[1:]
	}
	if a0() == "-nomain" { //wasm: skip main
		wg.NoMain, a = true, a[1:]
	}
	m := wg.Parse(a[0])
	if f {
		f77.F(os.Stdout, m)
	} else if k {
		m.K(os.Stdout)
	} else {
		m.Wat(os.Stdout)
	}
}
