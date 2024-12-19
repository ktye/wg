package wg

import (
	"bytes"
	"fmt"
	"strings"
	"testing"
)

func TestWg(t *testing.T) {

	{
		//printast = true
		m := Parse("x_test.go")
		if e := testGlobal(t, m.Globals); e != nil {
			t.Fatal(e)
		}
		for _, f := range m.Funcs {
			var buf bytes.Buffer
			f.Exported = m.Exports[f.Name]
			f.wat(&buf)
			got := trim(string(buf.Bytes()))
			exp := trim(f.Doc)
			if got != exp {
				t.Fatalf("func %s\ngot: %q\nexp: %q\n", f.Name, got, exp)
			}
		}
	}
}
func testGlobal(t *testing.T, g []Assign) error {
	g0 := g[0]
	if len(g0.Name) != 1 {
		return fmt.Errorf("expected 1 global assignment, got %d", len(g0.Name))
	}
	if g0.Name[0] != "pi" {
		return fmt.Errorf("expected global pi, got %s\n", g0.Name[0])
	}
	l := g0.Expr[0].(Literal)
	if l.Type != F64 {
		return fmt.Errorf("global pi: expected f64 got %v\n", l.Type)
	}
	if l.Value != "3.141592653589793" {
		return fmt.Errorf("global pi: expected 3.141592653589793 got %v\n", l.Value)
	}
	return nil
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
