
inline x = x

var x =   \v a l n b. v x
app x y = \v a l n b. a inline x y
lam x y = \v a l n b. l x y
num x =   \v a l n b. n x
bin x y = \v a l n b. b inline x y

dub = lam "x" (bin (var "x") (var "x"))
twice = lam "f1" (lam "x" (app (var "f1") (app (var "f1") (var "x"))))
prog1 = app (app twice dub) (num 10)

verr s = \e n c. e s
vnum x = \e n c. n x
vclose x env body = \e n c. c x env body

show v = v (\s. "error: " ^ s) (\n. "number:" ^ primInt2String n) (\_ _ _. "closure")

empty = \x . verr ("no binding for: " ^ x)
extend k v env = \x. (x===k) v (env x)
lookup k env = env k

appErr _ = verr "apply: expected closure for arg1"
apply eval v1 v2 = v1 verr appErr \x q body. eval body (extend x v2 q)

binErr1 _ _ _ = verr "binop: expected num for arg1"
binErr2 _ _ _ = verr "binop: expected num for arg2"
binop v1 v2 = v1 verr (\n1. v2 verr (\n2. vnum(n1+n2)) binErr2) binErr1

var1 q = \x. lookup x q
app1 q = \eval x y. apply eval (eval x q) (eval y q)
lam1 q = \x body. vclose x q body
num1 q = \n. vnum n
bin1 q = \eval x y. binop (eval x q) (eval y q)

u f = f f

eval = u \ev term q. term (var1 q) (\i.app1 q (i ev ev)) (lam1 q) (num1 q) (\i.bin1 q (i ev ev))
evalTop t = show (eval t empty)

evalTop (num 42)
evalTop (var "hey")
evalTop (bin (num 42) (num 11))
evalTop (lam "foo" (var "foo"))
evalTop (app (num 1) (num 2))
evalTop (app (lam "xx" (var "xx")) (num 9))
evalTop prog1
