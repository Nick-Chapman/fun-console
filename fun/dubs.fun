dub x = x + x
mul8 x = dub (dub (dub x))

mul8 11
dub (mul8 11)
