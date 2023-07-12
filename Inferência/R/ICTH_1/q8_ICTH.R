x = c(79, 78, 77, 76, 78, 85, 82, 80, 92, 78, 78, 79, 78, 78, 79, 80, 79, 78, 80, 83, 80, 80, 80, 79, 78, 70, 73, 72, 70, 78, 79, 80, 82, 79, 79)
xb = mean(x); xb
sd = sd(x); sd
n = length(x); n

# IC[mu; 95%]
gama = 0.95; alfa = 1-gama; gama; alfa; alfa/2
ztab = qnorm(1-alfa/2); round(ztab,2)
e = ztab*sd/sqrt(n); e
IC_95 = xb+c(-1,1)*e; IC_95

# IC[mub - mua; 90%]
gama = 0.9; alfa = 1-gama; gama; alfa; alfa/2
xb2 = 82
n2 = 36
sd2 = 5

# considerando as variâncias iguais, para populações independentes

S2p = ((n-1)*sd^2 + (n2-1)*sd2^2)/(n+n2-2); S2p
D = xb2 - xb; D
ttab = qt(1-alfa/2,n+n2-2); ttab
erro = ttab*sqrt(S2p)*sqrt((1/n) + (1/n2)); erro
IC_90 = D+c(-1,1)*erro; IC_90

# como 1 não pertence ao intervalo e o mesmo é positivo
# rejeitamos H0: "as médias são iguais", e consideramos
# que sim, o tratamento B tem umidade média maior!!!