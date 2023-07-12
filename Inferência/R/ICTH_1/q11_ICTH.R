antes = c(2.20, 1.95, 2.10, 2.05, 2.10, 1.98, 2.08, 2.10, 2.12, 2.20, 2.15, 2.20, 2.15, 2.20)
depois = c(2.43, 2.40, 2.05, 2.25, 2.05, 2.32, 2.30, 2.05, 2.20, 2.28, 2.35, 2.40, 2.40, 2.35)
media_antes = mean(antes); sdAntes = sd(antes); media_antes; sdAntes
media_depois = mean(depois); sdDepois = sd(depois); media_depois; sdDepois
n = length(antes); n
m = length(depois); m

# os dados são normalmente distribuídos?
shapiro.test(antes)$p.value
shapiro.test(depois)$p.value
# a partir do teste, os dados de ANTES são normalmente distribuídos, mas os de DEPOIS, não

# as variâncias populacionais são iguais ou distintas?
rp = max(sdAntes^2, sdDepois^2)/min(sdAntes^2,sdDepois^2); rp; rp<4
# consideramos que as variâncias populacionais são iguais pelo teste prático

# H0: mu2-mu>0.1 H1: mu2-mu1<=0.1
# vamos construir um IC unilateral para a diferença das médias populacionais com as hipóteses acima, considerando as variâncias iguais e desconhecidas
alfa = 0.05; gama=1-alfa; gama; alfa; alfa/2

Sp2 = ((n-1)*sdAntes^2 + (m-1)*sdDepois^2)/(n+m-2); Sp2
ttab = qt(1-alfa, n+m-2); ttab
e = ttab*sqrt(Sp2)*sqrt(1/n + 1/m); e
d = media_depois - media_antes; d
ls = d+e;
IC95 = c(-Inf,ls); IC95; round(IC95,2)
li = d-e;
IC95 = c(li, Inf); IC95

# vimos que 0.1 está no intervalo
# portanto, rejeitamos H0, pois não há evidências para supor que mu2-mu1>0.1!!!

# H0: mu2<=2.4 H1: mu2>2.4
# vamos construir um IC unilateral para mu2, com 95% de confiança, variância desconhecida
ttab = qt(1-alfa,m-1); ttab
e = ttab*sdDepois/sqrt(m); e
ls = media_depois+e; ls; round(ls,2)
li = media_depois-e; li; round(li,2)
IC95_uni_sup = c(-Inf, ls); IC95_uni_sup; round(IC95_uni_sup,2)
IC95_uni_inf = c(li, Inf); IC95_uni_inf; round(IC95_uni_inf,2)


ttab = qt(1-alfa/2,m-1); ttab;
e = ttab*sdDepois/sqrt(m); e
IC95_bi = media_depois+c(-1,1)*e; round(IC95_bi,2)

# logo, rejeitamos H0, já que o máximo, dado os IC construídos, é 2,35Kg!!!

a = table(depois>2.3); a
s = 7; p_est = s/m; p_est # s são m ensaios de Bernouli, com p=s/m

# H0: p>0.7 H1: p<=0.7
# vamos construir dois intervalos: unilateral e bilateral
ztab = qnorm(1-alfa); ztab
e = ztab*sqrt(p_est*(1-p_est)/m); e
li = p_est-e; li
ls = p_est+e;ls
IC_uni_i = c(li,1); IC_uni_i
IC_uni_s = c(0,ls); IC_uni_s
ztab2 = qnorm(1-alfa/2); ztab2
e2 = ztab*sqrt(p_est*(1-p_est)/m); e2
IC95_bi_p = p_est+c(-1,1)*e2; IC95_bi_p

# portanto, rejeitamos H0, pois 0.7 está presente nos intervalos!!!

# como seria fazer direto no R?
binom.test(s,m,p_est,alternative = "greater")
binom.test(s,m,p_est, alternative = "less")
binom.test(s,m,p_est)
