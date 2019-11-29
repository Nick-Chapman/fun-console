
u f = f f

true = \t f. t u
false = \t f. f u
showB b = b (\u."true") (\u."false")

--zero z _ = z
--succ x _ s = s u x
--showN = u \showN n. n 0 \u p. 1 + u showN p
--add = u \add a b. a b \u a. succ (u add a b)
--greater = u \greater a b. a false \u a. b true \u b. u greater a b

zero = 0
succ x = x+1
add x y = x+y
greater x y = if (x>y) true false
showN n = n

one = succ zero
two = succ one
three = succ two
four = succ three
five = succ four

--lists...
nil n c = n
cons x xs n c = c u x xs
map = u \map f xs. xs nil \u x xs. cons (f x) (u map f xs)
fold = u \fold b f xs. xs b \u x xs. f x (u fold b f xs)
upto = u \upto a b. (greater a b) (\u.nil) (\u.cons a (u upto (add a one) b))
sum = fold zero add

dub x = add x x
foo n = showN (sum (map dub (upto one n)))
foo five
