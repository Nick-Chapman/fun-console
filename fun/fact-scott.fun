
-- Factorial, using scott encoded naturals

u x = x x

zero z _ = z
succ x _ s = s x u
one = succ zero

unscott = u \show n. n 0 \n u. 1 + u show n
add = u \add a b. a b \a u. succ (u add a b)
mul = u \mul a b. a zero \a' u. add b (u mul a' b)
fact = u \fact n. n one \p u. mul n (u fact p)

six = succ (succ (succ (succ (succ (succ zero)))))

fact' x = unscott (fact x)

fact' six
