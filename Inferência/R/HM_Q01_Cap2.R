getwd()
set.seed(508492)

A = rnorm(100, 0, 4);round(A, 2)
A0 = sort(A);round(A0, 2)

mean(A0);var(A0)

s = sum(A^2);s
n = 100
t = s/n;t

Vt = 2*t^4/n;Vt
ep_Vt = sqrt(Vt);ep_Vt
