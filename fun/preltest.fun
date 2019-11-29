
-- non primitives
not b = if b false true

assert what b = if b 0 (error ("assert-failed:" ^ what))


0+assert "true" (true)
0+assert "not false" (not false)

--0+assert "false" (false) -- expect fail
--0+assert "not true" (not true) -- expect fail

0+assert "==" (1==1)
0+assert "===" ("xy"==="xy")

0+assert "+" (1+2==3)
0+assert "-" (3-2==1)
0+assert "*" (2*3==6)
0+assert "^" ("ab"^"cd"==="abcd")

0+assert "int2string" (int2string 42 === "42")

0+assert ">12"  (not (1>2))
0+assert ">=12" (not (1>=2))
0+assert "<12"  (1<2)
0+assert "<=12" (1<=2)

0+assert ">22"  (not (2>2))
0+assert ">=22" (2>=2)
0+assert "<22"  (not (2<2))
0+assert "<=22" (2<=2)

0+assert ">21"  (2>1)
0+assert ">=21" (2>=1)
0+assert "<21"  (not (2<1))
0+assert "<=21" (not (2<=1))


--ni x = noinline x
--err s = error s
--\_. err "what"
--\_. ni 42
