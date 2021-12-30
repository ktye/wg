package example

import . "github.com/ktye/wg/module"

// alloc_init sets the free list for the buddy allocator.
// e.g. alloc_init(5, 16) prepares 64kB
func alloc_init(a, b int32) {
	p := int32(1 << a)
	for a < b {
		SetI32(4*a, p)
		SetI32(p, 0)
		p *= 2
		a++
		continue
	}
	SetI32(128, b)
}

// alloc returns the address of a block for at least n bytes of memory.
// the address is 16 bytes alligned, I32(r-16) stores the bucket type
// and 12 following bytes are unused.
func alloc(n int32) int32 {
	t := alloc_bucket(n)
	i := 4 * t
	m := 4 * I32(128) // current max allocation is stored at 128
	for I32(i) == 0 {
		if i >= m {
			m = 4 * alloc_grow(i)
		} else {
			i += 4
		}
	}
	a := I32(i)
	SetI32(i, I32(a))
	for j := i - 4; j >= 4*t; j -= 4 {
		u := a + 1<<(j>>2)
		SetI32(u, I32(j))
		SetI32(j, u)
	}
	SetI32(a, t)
	return 16 + a
}
func alloc_bucket(n int32) (r int32) {
	r = 32 - I32clz(uint32(15+n))
	if r < 5 {
		return 5
	} else {
		return r
	}
}
func alloc_grow(p int32) int32 {
	m := I32(128)                       // old total memory (log2)
	n := 1 + (p >> 2)                   // required total mem (log2)
	g := (1 << (n - 16)) - Memorysize() // grow by 64k blocks
	if g > 0 {
		Memorygrow(g) // error: <0
	}
	alloc_init(m, n)
	return n
}
func free(p int32) {
	t := 4 * I32(p-16)
	SetI32(p, I32(t))
	SetI32(t, p-16)
}
