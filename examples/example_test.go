package example

import (
	"testing"

	"github.com/ktye/wg/module"
)

func TestReduce(t *testing.T) {
	module.Memory(1)
	module.SetF64(0, 1)
	module.SetF64(8, 2)
	module.SetF64(16, 3)
	r := reduce(0, 0, 3)
	if r != 6 {
		t.Fatalf("expected 6, got %v", r)
	}
}
