package main

import (
	"os"

	"github.com/ktye/wg"
)

func main() {
	a := os.Args[1:]
	if a[0] == "-try" {
		wg.TryCatch = true
		a = a[1:]
	}
	if a[0] == "-multi" {
		wg.MultiMemory = true
		a = a[1:]
	}
	m := wg.Parse(a[0])
	m.Wat(os.Stdout)
}
