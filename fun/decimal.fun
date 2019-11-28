
id x = x
y f = noinline (\x. f (x x)) (\x. f (x x))

nil n c = n
cons x xs n c = c x xs

b0 z o = z
b1 z o = o

not b = b b1 b0
and x y = x b0 y

d0 a b c d e f g h i j = a
d1 a b c d e f g h i j = b
d2 a b c d e f g h i j = c
d3 a b c d e f g h i j = d
d4 a b c d e f g h i j = e
d5 a b c d e f g h i j = f
d6 a b c d e f g h i j = g
d7 a b c d e f g h i j = h
d8 a b c d e f g h i j = i
d9 a b c d e f g h i j = j

dshow d = d "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
dsshow = y \dsshow ds. ds "d" \d ds. dsshow ds ^ dshow d

x0 r = r d1 b0
x1 r = r d2 b0
x2 r = r d3 b0
x3 r = r d4 b0
x4 r = r d5 b0
x5 r = r d6 b0
x6 r = r d7 b0
x7 r = r d8 b0
x8 r = r d9 b0
x9 r = r d0 b1
dinc d = d x0 x1 x2 x3 x4 x5 x6 x7 x8 x9
dsinc = y \dsinc ds. ds (cons d1 nil) \d ds. dinc d \d b. cons d (b id dsinc ds)

x0 r = r d0 b0
x1 r = r d2 b0
x2 r = r d4 b0
x3 r = r d6 b0
x4 r = r d8 b0
x5 r = r d0 b1
x6 r = r d2 b1
x7 r = r d4 b1
x8 r = r d6 b1
x9 r = r d8 b1
ddub d = d x0 x1 x2 x3 x4 x5 x6 x7 x8 x9
dsdub = y \dsdub ds. ds nil \d ds. ddub d \d b. cons d (b id dsinc (dsdub ds))

bsinc = y \bsinc bs. bs (cons b1 nil) \b bs. b (cons b1 bs) (cons b0 (bsinc bs))
fa x y c = \r. x (y (r c b0) (r (not c) c)) (y (r (not c) c) (r c b1))
bsadd = y (\bsadd c xs ys. xs (c id bsinc ys) \x xs'. ys (c id bsinc xs) \y ys'. fa x y c \b c. cons b (bsadd c xs' ys')) b0

bsX2 = cons b0
bsX4 bs = bsX2 (bsX2 bs)
bsmul = y \recurse xs ys. xs nil \x xs'. ys nil \y ys'. ((and x y) id bsinc) ((x id (bsadd (bsX2 ys'))) ((y id (bsadd (bsX2 xs'))) (bsX4 (recurse xs' ys'))))

ds0 = nil
bs2ds = y \conv bs. bs ds0 \b bs. b id dsinc (dsdub (conv bs))

bs0 = nil
bs1 = bsinc bs0
bs2 = bsinc bs1
bs3 = bsinc bs2
bs4 = bsinc bs3

none   n j = n
just x n j = j x

bsdec = y \rec bs. bs none \b bs. b (rec bs none \bs. just (cons b1 bs)) (just (cons b0 bs))

bsfact = y \rec n. bsdec n bs1 \n'. bsmul n (rec n')

dsshow (bs2ds (bsfact (bsmul bs3 bs3)))
