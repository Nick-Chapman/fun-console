dub x = x + x
mul8 x = dub (dub (dub x))
res = mul8 1
res
dub res
sub x = x - 1
thrice f x = f (f (f x))
tts = thrice thrice sub
tts 0
