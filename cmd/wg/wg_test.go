package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestWg(t *testing.T) {

	//printast = true
	m := parse("x.go")

	for _, f := range m.Funcs {
		var buf bytes.Buffer
		f.wat(&buf)
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
	s = strings.Replace(s, "  ", " ", -1)
	s = strings.Replace(s, "  ", " ", -1)
	s = strings.Replace(s, " )", ")", -1)
	return s
}
