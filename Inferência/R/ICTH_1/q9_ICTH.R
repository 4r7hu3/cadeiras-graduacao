n = 21; xbarraA = 7.68;  SA = 0.0004; m = 21; xbarraB = 7.24; SB = 0.0025
rp = max(SA,SB)/min(SA,SB); rp; rp<4
# analisando pela regra prática, temos que as variâncias são distinstas
# vamos construir um IC para a razão de variâncias, com 90% de confiança
alfa = 0.1; gama = 1-alfa; gama; alfa; alfa/2
razao = SA/SB; razao
# H0: sigma1^2 = sigma2^2 H1: sigma1^2!=sigma2^2

f2 = qf(0.95,n-1,m-1); f2
f1 = qf(0.05,n-1,m-1); f1
IC_90 = c(f1*razao,f2*razao); IC_90; round(IC_90,2)

# dado o IC, rejeitamos H0 e consideramos que as variâncias são distintas!!!
# agora vamos calcular o IC para a diferença das médias, considerando duas populações independentes, com variâncias distintas e desconhecidas

alfa2 = 0.05; gama2=1-alfa2; gama2; alfa2; alfa2/2

A = SA/n; B = SB/m; A; B
r = ((A+B)^2)/((A^2)/(n-1) + (B^2)/(m-1)); r
ttab = qt(1-alfa2/2,round(r)); ttab; round(ttab,2)
d = xbarraA - xbarraB; d
e = ttab*sqrt(SA/n + SB/m); e
IC_95 = d+c(-1,1)*e; IC_95

# rejeitamos H0 e consideramos que o Ph da primeira solução é maior do que o da segunda, pois IC é positivo