n = 120; s1 = 35
m = 90; s2 = 30
# temos duas populações independentes
p_est1 = s1/n; p_est2 = s2/m; p_est1; p_est2
# temos dois ensaios de Bernouli, com n e m repetições e p estimado em p_est1 e p_est2
# queremos calcular a diferença de proporções
# H0: p_2<p1 H1: p>=p1
alfa = 0.05; gama=1-alfa; gama; alfa; alfa/2
d = p_est2 - p_est1; d
ztab = qnorm(1-alfa); ztab
aux = sqrt((p_est1*(1-p_est1))/n + (p_est2*(1-p_est2))/m); aux
e = ztab*aux; e
li = d-e; li
IC95 = c(li, 1); IC95
ls = d+e;
IC95 = c(0, ls); IC95
IC95_bi = c(li,ls); IC95_bi
# logo, rejeitamos H0, não havendo portanto uma redução