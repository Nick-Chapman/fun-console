dub x = x + x
mul8 x = dub (dub (dub x))
res = mul8 1
res
dub res
