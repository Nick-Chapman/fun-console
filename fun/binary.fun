
id x = x
y = \f. (\x. f (x x)) (\x. f (x x))

false = \f t. f
true  = \f t. t

none = \n j. n
just x = \n j. j x

pOne    = \o d. o
pDub p  = \o d. d false p
pDub1 p = \o d. d true  p

zero      = \z nz. z
nonZero p = \z nz. nz p

dub x = x + x
pNum = y \num p. p 1 \b h. dub (num h) + b 0 1
num n = n 0 pNum

pInc = y \inc p. p (pDub pOne) \b h. b (pDub1 h) (pDub (inc h))
inc n = nonZero (n pOne pInc)
one = inc zero

pDub2 p = pDub (pInc p)
pAdd = y \add p1 p2. p1 (pInc p2) \b1 h1. p2 (pInc p1) \b2 h2. b1 (b2 pDub pDub1) (b2 pDub1 pDub2) (add h1 h2)
add n1 n2 = n1 n2 \p1. n2 n1 \p2. nonZero (pAdd p1 p2)

nDub n = n zero \p. nonZero (pDub p)
nDub1 n = n one \p. nonZero (pDub1 p)
pDec = y \dec p. p zero \b h. b (nDub1 (dec h)) (nonZero (pDub h))
decOpt n = n none \p. just (pDec p)

px4 p = pDub (pDub p)
pMul = y \mul p1 p2. p1 p2 \b1 h1. p2 p1 \b2 h2. b1 (b2 (px4 (mul h1 h2)) (pAdd (px4 (mul h1 h2)) p1)) (b2 (pAdd (px4 (mul h1 h2)) p2) (pAdd (px4 (mul h1 h2)) (pAdd (pDub h1) p2)))
mul n1 n2 = n1 zero \p1. n2 zero \p2. nonZero (pMul p1 p2)

fact = y \fact n. decOpt n one \n'. mul n (fact n')

n0 = zero
n1 = inc n0
n2 = inc n1
n3 = inc n2
n4 = inc n3

num (add n3 n4)
num (mul n3 n4)
num (fact n4)
num (fact (fact n3))
