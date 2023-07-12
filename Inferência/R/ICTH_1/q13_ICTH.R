n = 100; s1 = 85; m = 200; s2 = 120;
p_est1 = s1/n; p_est2 = s2/n; p_est1; p_est2
alfa = 0.1; gama = 1-alfa; gama; alfa; alfa/2
# H0: p2>p1 H1: p2<=p1
ztab = qnorm(1-alfa/2); ztab
aux = sqrt((p_est1*(1-p_est1))/n + (p_est2*(1-p_est2))/m); aux
d = p_est2-p_est1; d
e = ztab*aux; e
ls = d+e; ls
IC90 = c(0,ls); IC90
li = d-e; li
IC_bi = c(li, ls); IC_bi
# não rejeitamos H0