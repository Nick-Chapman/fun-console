
y f = noinline (\x. f (x x)) (\x. f (x x))

zero z _ = z
succ x _ s = s x

one = succ zero
two = succ one
three = succ two

show = y \show n. n 0 \p. 1 + show p

add = y \add a b. a b \a. succ (add a b)
mul = y \mul a b. a zero \a'. add b (mul a' b)
fact = y \fact n. n one \p. mul n (fact p)

show (fact (fact three))

res = fact three
res
