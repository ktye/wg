package wg

import (
	"bytes"
	"strings"
	"testing"
)

func TestWg(t *testing.T) {

	//printast = true
	m := Parse("x_test.go")

	for _, f := range m.Funcs {
		var buf bytes.Buffer
		w := newIndent(&buf)
		f.wat(w)
		w.Write(nil)
		got := trim(string(buf.Bytes()))
		exp := trim(f.Doc)
		if got != exp {
			t.Fatalf("func %s\ngot: %q\nexp: %q\n", f.Name, got, exp)
		}
	}
}
func trim(s string) string {
	s = strings.Replace(s, "\n", " ", -1)
	s = strings.TrimSpace(s)
	for {
		n := len(s)
		s = strings.Replace(s, "  ", " ", -1)
		if len(s) == n {
			break
		}
	}
	s = strings.Replace(s, " )", ")", -1)
	return s
}
