package wg

func Walk(root Emitter, f func(e Emitter) bool) { root.walk(f) }

func (x Stmts) walk(f func(e Emitter) bool) {
	if f(x) {
		for i := range x {
			x[i].walk(f)
		}
	}
}
func (x Nop) walk(f func(e Emitter) bool) { f(x) }
func (x Return) walk(f func(e Emitter) bool) {
	if f(x) {
		for i := range x {
			x[i].walk(f)
		}
	}
}
func (x Binary) walk(f func(e Emitter) bool) {
	if f(x) {
		x.X.walk(f)
		x.Y.walk(f)
	}
}
func (x Unary) walk(f func(e Emitter) bool) {
	if f(x) {
		x.X.walk(f)
	}
}
func (x LocalGets) walk(f func(e Emitter) bool)  { f(x) }
func (x LocalGet) walk(f func(e Emitter) bool)   { f(x) }
func (x GlobalGets) walk(f func(e Emitter) bool) { f(x) }
func (x GlobalGet) walk(f func(e Emitter) bool)  { f(x) }
func (x Assign) walk(f func(e Emitter) bool) {
	if f(x) {
		for i := range x.Expr {
			x.Expr[i].walk(f)
		}
	}
}
func (x Branch) walk(f func(e Emitter) bool)  { f(x) }
func (x Literal) walk(f func(e Emitter) bool) { f(x) }
func (x If) walk(f func(e Emitter) bool) {
	if f(x) {
		x.If.walk(f)
		x.Then.walk(f)
		if x.Else != nil {
			x.Else.walk(f)
		}
	}
}
func (x Switch) walk(f func(e Emitter) bool) {
	if f(x) {
		x.E.walk(f)
		for i := range x.Case {
			x.Case[i].walk(f)
		}
		if x.Def != nil {
			x.Def.walk(f)
		}
	}
}
func (x For) walk(f func(e Emitter) bool) {
	if f(x) {
		if x.Cond != nil {
			x.Cond.walk(f)
		}
		if x.Post != nil {
			x.Post.walk(f)
		}
		if x.Body != nil {
			x.Body.walk(f)
		}
	}
}
func (x Cast) walk(f func(e Emitter) bool) {
	if f(x) {
		x.Arg.walk(f)
	}
}
func (x Call) walk(f func(e Emitter) bool) {
	if f(x) {
		for i := range x.Args {
			x.Args[i].walk(f)
		}
	}
}
func (x CallIndirect) walk(f func(e Emitter) bool) {
	if f(x) {
		x.Func.walk(f)
		for i := range x.Args {
			x.Args[i].walk(f)
		}
	}
}
func (x Printf) walk(f func(e Emitter) bool) { f(x) }
