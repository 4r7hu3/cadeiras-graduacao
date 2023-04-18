set.seed(32)

A = rbinom(100, 2, 0.5);A
table(A)
s = sum(A);s
xb = mean(A);xb

t = xb/2;t

n = 100
Vt = t*(1-t)/(2*n);Vt
ep_t = sqrt(Vt);ep_t
