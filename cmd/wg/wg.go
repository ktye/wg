package main

import (
	"os"

	"github.com/ktye/wg"
)

func main() {
	a := os.Args[1:]
	c := false
	if a[0] == "-try" {
		wg.TryCatch = true
		a = a[1:]
	}
	if a[0] == "-multi" {
		wg.MultiMemory = true
		a = a[1:]
	}
	if a[0] == "-c" {
		c = true
		a = a[1:]
	}
	if a[0] == "-prefix" { // -c only prefix symbols with a[1]
		wg.Prefix = a[1]
		a = a[2:]
	}
	if a[0] == "-nomain" { // -c only skip main
		wg.Nomain = true
		a = a[1:]
	}
	m := wg.Parse(a[0])
	if c {
		m.C(os.Stdout)
	} else {
		m.Wat(os.Stdout)
	}
}
