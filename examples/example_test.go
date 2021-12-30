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

// alloc.go

func TestAlloc(t *testing.T) {
	for i := int32(0); i < 64*1024; i += 4 {
		module.SetI32(i, 0)
	}
	alloc_init(5, 16)
	r := alloc(1000)
	if r != 1040 {
		t.Fatalf("r=%d\n", r)
	}
	if m := module.I32(r - 16); m != 10 {
		t.Fatalf("I[r-16] = %d should be 10", m)
	}
	for i := int32(0); i < 32; i++ {
		//fmt.Printf("I32[%d] = %d\n", 4*i, module.I32(4*i))
	}
	if m := module.I32(40); m != 0 {
		t.Fatalf("I32[40] = %d, should be 0\n", m)
	}
	if i := module.I32(128); i != 16 {
		t.Fatalf("I32(128): %v != 16", i)
	}
	free(r)
	if m := module.I32(40); m != 1024 {
		t.Fatalf("I32[40] = %d, should be 1024\n", m)
	}
}
func TestBucket(t *testing.T) {
	tc := []struct {
		n, t int32
	}{
		{0, 5},
		{1, 5},
		{2, 5},
		{16, 5},
		{17, 6},
		{24, 6},
		{48, 6},
		{49, 7},
	}
	for _, c := range tc {
		s := alloc_bucket(c.n)
		if s != c.t {
			t.Fatalf("alloc_bucket(%d) = %d not %d\n", c.n, s, c.t)
		}
		n := int32(1) << s
		if c.n > n-16 {
			t.Fatalf("alloc_bucket(%d) has space for only %d bytes\n", c.n, n-8)
		}
		m := int32(1) << (s - 1)
		if s > 5 && c.n+16 <= m {
			t.Fatalf("alloc_bucket(%d) returns %d, but %d would have been enough (type %d)\n", c.n, s, s-1, m)
		}
	}

}
