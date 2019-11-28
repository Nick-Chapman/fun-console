y f = noinline (\x. f (x x)) (\x. f (x x))
err = y \x.x
nil n c = n
cons x xs n c = c x xs
head xs = xs err \x xs. x
sam = cons 1 (cons 2 nil)
ones = y \ones. cons 1 ones
length = y \length xs. xs 0 \x xs. 1 + length xs
sum = y \sum xs. xs 0 \x xs. x + sum xs
sum sam
