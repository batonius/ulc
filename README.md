# ULC

(un)Typed Lambda Calculus beta-reducer/type checker implemented in Rust.

Implements strict and lazy beta-reduction for untyped lambda calculus and type checking for λ2 + λω.

# Syntax
Basic lambda calculus with \ for λ, extended with numbers, booleans(true and false), conditionals (if .. then .. else ..) and type-valued terms ([Type]). On type level \ stands for ∀, -> for →, {A B} for type application; on kind level * stands for ∗ and => for ⇒. "let x:T = y in z" is a shorthand for (\x:T.z) y.


# Examples
```
> (\f.(\x.f (\v.(x x v))) (\x.f (\v.(x x v)))) (\f.\n.(if (= n 1) then 1 else (* n (f (- n 1))))) 20
2432902008176640000
	: Type check failed.
```
```
> (\a:*=>*.\b:*.\x:{a ({a b})}.x) 
\a:*=>*.\b:*.\x:{a {a b}}.x
	: \a:*=>*.\b:*.({a {a b}} -> {a {a b}})
```
```
> (\a:*=>*.\b:*.\x:{a ({a b})}.x) [\x:*.x->x] 
\b:*.\x:{a {a b}}.x
	: \b:*.({\x:*.(x -> x) {\x:*.(x -> x) b}} -> {\x:*.(x -> x) {\x:*.(x -> x) b}})
```
```
> (\a:*=>*.\b:*.\x:{a ({a b})}.x) [\x:*.x->x] [Int]
\x:{a {a b}}.x
	: (((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
```
```
> a:{{(\a:*=>*.\b:*.{a ({a b})}) (\x:*.x->x)} Int}
a
	: ((Int -> Int) -> (Int -> Int))
```
```
> let inc:Int->Int=\x:Int.(+ x 1) in let dup:\t:*.(t->t)->t->t=\t:*.\f:t->t.\x:t.f (f x) in dup [Int] inc 10
12
	: Int
```
