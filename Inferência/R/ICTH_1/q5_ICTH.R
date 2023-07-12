nA = 31; xbA = 1290; sdA = 200
nB = 25; xbB = 980; sdB = 100
alfa = 0.05; gama=1-alfa; gama; alfa; alfa/2

# vamos construir um IC para a razão de variâncias
rp = max(sdA^2,sdB^2)/min(sdA^2,sdB^2); rp; rp<4
# a princípio, pelo teste prático, as variâncias são distintas

razao = sdA^2/sdB^2; razao; round(razao,2)

f2 = qf(0.975,30,24); f2; round(f2,2) # P(F(30,24) > f2) = 0.05
f1 = qf(0.025,30,24); f1; round(f1,2) # P(F(30,24) < f1) = 0.05
IC_95 = c(f1*razao,f2*razao); IC_95; round(IC_95,2)
# portanto, rejeitamos a hipótese de que sigma1^2 = sigma2^2, pois 1 não pertence ao intervalo

# vamos construir um IC para a diferença das médias, com 90% de confiança, populações independentes e variâncias distintas desconhecidas

alfa2 = 0.1; gama2 = 1-alfa2; gama2; alfa2; alfa2/2

# H0: muA>muB H1: muA<=muB, que é equivalente a H0: muA=muB H1: muA!=muB
# queremos um IC[mu_a - mu_b; 90%]
# para isso, usaremos a distribuição t, para o caso de populações independentes
# e em que sigma1^2 != sigma2^2

d = xbA - xbB; d
A = sdA^2/nA; A
B = sdB^2/nB; B
r = ((A + B)^2)/(A^2/(nA-1) + B^2/(nB-1)); r
r = round(r)
ttab = qt(1-alfa2/2,r); ttab
e = ttab*sqrt((sdA^2)/nA + (sdB^2)/nB); e
IC_90 = d+c(-1,1)*e; IC_90; round(IC_90,2)
# como os valores do intervalo são positivos, não rejeitamos H0