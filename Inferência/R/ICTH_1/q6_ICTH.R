x = c(140,135,120,126,127,128,140,145,143,145,142,143,140,145,150,130,160,155,155,160,161,160,145,140,140)
n = length(x); n
xb = mean(x); xb; s = sd(x); s

# IC[mu; 95%]
# como n=25 e sigma é desconhecido, então usamos t(24)

alfa = 0.05; gama = 1-alfa; gama; alfa; alfa/2
ttab = qt(1-alfa/2,n-1); ttab; round(ttab,2)
e = ttab*s/sqrt(n); e
IC_95 = xb+c(-1,1)*e; IC_95; round(IC_95,2)
e_desejado = 1 
n_procurado = ceiling((ttab*s/e_desejado)^2); n_procurado

# intervalo de confiança para a proporção de canteiros com produção >140Kg/Ha, com alfa=1%
a = table(x>140); a
test = binom.test(14,n,p=14/n,conf.level = 0.99); test$conf.int # intervalo exato
p_est = 14/n; p_est
ztab = qnorm(1-0.01/2); ztab; round(ztab,2)
e_prop = ztab*sqrt(p_est*(1-p_est)/n); e
IC_99_prop = p_est+c(-1,1)*e_prop; IC_99_prop; round(IC_99_prop,2) # intervalo usando MV

m = 50; xb2 = 150; s_2 = 10; alfa=0.1; gama = 1-alfa; gama; alfa; alfa/2

# primeiro, vamos saber, usando a razão de variâncias, se as variâncias são iguais (H0 = as variâncias são iguais)
rp = max(s^2,s_2^2)/min(s^2,s_2^2); rp; rp<4
# pela regra prática, consideramos que as variâncias são iguais
# vamos construir um IC unilateral de 90% para a diferença de médias
# H0: mu1 = mu2; H1: mu2>mu1

Sp2 = ((n-1)*s^2 + (m-1)*s_2^2)/(n+m-2); Sp2
D = xb2- xb; D
gl = n+m-2; gl
ttab = qt(gama,gl); ttab; round(ttab,2)
erro = ttab*sqrt(Sp2)*sqrt(1/n + 1/m); erro
IC = c(D-erro, Inf); IC; round(IC,2)
# como 0 não pertence ao intervalo, rejeitamos H0, e temos que mu2>mu1


# H0: p>=0.6 H1: p<0.6
# o que é equivalente a H0: p=0.6 H1: p!=0.6
ztab = qnorm(1-0.05/2); ztab
erro = ztab*sqrt(p_est*(1-p_est)/n); erro
IC95_p = p_est+c(-1,1)*erro; IC95_p; round(IC95_p,2)
# como 0.6 pertence ao intervalo, concluímos que nossa suposição está correta