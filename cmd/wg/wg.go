package main

import (
	"os"

	"github.com/ktye/wg"
	"github.com/ktye/wg/f77"
)

func main() {
	a := os.Args[1:]
	c := false
	f := false
	k := false
	if a[0] == "-try" {
		wg.TryCatch, a = true, a[1:]
	}
	if a[0] == "-multi" {
		wg.MultiMemory, a = true, a[1:]
	}
	if a[0] == "-c" { // C
		c, a = true, a[1:]
	}
	if a[0] == "-f" { // f77
		f, a = true, a[1:]
	}
	if a[0] == "-k" {
		k, a = true, a[1:]
	}
	if a[0] == "-prefix" { // -c only prefix symbols with a[1]
		wg.Prefix = a[1]
		a = a[2:]
	}
	if a[0] == "-nomain" { // -c only skip main
		wg.Nomain, a = true, a[1:]
	}
	m := wg.Parse(a[0])
	if c {
		m.C(os.Stdout)
	} else if f {
		f77.F(os.Stdout, m)
	} else if k {
		m.K(os.Stdout)
	} else {
		m.Wat(os.Stdout)
	}
}
