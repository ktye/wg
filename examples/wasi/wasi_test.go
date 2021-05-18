package wasi_example

import (
	"testing"

	"github.com/ktye/wg"
)

func TestWg(t *testing.T) {
	wg.Parse("wasi.go")
}
