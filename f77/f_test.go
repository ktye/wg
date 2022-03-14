package f77

import (
	"os"
	"testing"

	"github.com/ktye/wg"
)

func TestF(t *testing.T) {
	m := wg.Parse("x_test.go")
	F(os.Stdout, m)
}
