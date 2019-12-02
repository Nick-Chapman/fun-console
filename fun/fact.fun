
-- Factorial (using builtin numbers)

y = \f. noinline (\x. f (x x)) (\x. f (x x))

fact = y \fact n. if (n==0) 1 (n * fact (n-1))

fact 6
