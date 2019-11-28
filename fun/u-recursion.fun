
u f = f f
inline x = x

zero z _ = z
succ x _ s = s inline x

one = succ zero
two = succ one
three = succ two

show = u \show n. n 0 \i p. 1 + i show show p

add = u \add a b. a b \i a. succ (i add add a b)
mul = u \mul a b. a zero \i a'. add b (i mul mul a' b)
fact = u \fact n. n one \i p. mul n (i fact fact p)

show (fact (fact three))

res = fact three
res
