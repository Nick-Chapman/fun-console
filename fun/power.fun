
u f = f f

error _ = 999 -- TODO: investigate elim of this hack

true = \t f. t u
false = \t f. f u
showB b = b (\u."true") (\u."false")


-- unary numbers...
--zero z _ = z
--succ x _ s = s u x
--showN = u \showN n. n 0 \u p. 1 + u showN p
--add = u \add a b. a b \u a. succ (u add a b)
--sub = u \sub a b. b a \u b. a (error "negative") \u a. u sub a b
--mul = u \mul a b. a zero \u a'. add b (u mul a' b)
--eq = u \eq a b. a (b true \u _.false) \u a. b false \u b. u eq a b



-- builtin numbers...
zero = 0
succ x = x+1
showN n = n
add x y = x+y
sub x y = x-y
mul x y = x*y
eq x y = if (x==y) true false


one = succ zero
two = succ one
three = succ two
four = succ three
five = succ four

power = u \power x n. (eq n zero) (\u. one) \u. mul x (u power x (sub n one))

pow2 = power two
pow2 three
showN (pow2 three)

cube x = power x three
cube three
showN (cube three)
