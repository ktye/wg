package main

import (
	"os"

	"github.com/ktye/wg"
)

func main() {
	m := wg.Parse(os.Args[1])
	m.Wat(os.Stdout)
}
