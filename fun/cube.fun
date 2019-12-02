
-- Cube, as a specializatioon of power (using builtin numbers)


delta x = x x

casenum n z s = (if (n==0) (\_.z) (s (n-1))) delta

power = delta \power x n. casenum n 1 \n' delta. x * (delta power x n')

cube x = power x 3
cube 7

exp = power 10
exp 5

res = power 2 9
