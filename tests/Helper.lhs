> module Helper where


An infix conditional expression.  Using this, `c ? t $ e` resembles
other language's ternary operator `c ? t : e`.

> infix 1 ?
> (?) :: Bool -> t -> t -> t
> c ? x = \y-> if c then x else y
