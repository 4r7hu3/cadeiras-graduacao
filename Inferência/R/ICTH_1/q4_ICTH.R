x = c(135,140,155,180,165,200,147,181,173,203,145,195,178,187)/100; x
xb = mean(x); s = sd(x)
xb; s; round(xb,2); round(s,2)
n = length(x); n
gama = 0.95; alfa=1-gama; gama; alfa; alfa/2

# como n=14 e sigma é desconhecido, usamos a distribuição t(n-1) = t(13), pois 13 tem na tabela da t

ttab = qt(1-alfa/2,n-1); ttab; round(ttab,2)
e = ttab*s/sqrt(n); e; round(e,2)
IC_95 = xb+c(-1,1)*e; IC_95; round(IC_95,2)
e_desejado = 0.05

n2 = ceiling((ttab*s/e_desejado)^2); n2

# H0: mu>=1.8 H1: mu<1.8
# isso é equivalente a H0: mu=1.8 H1: mu!=1.8
# já vimos, pelo intervalo anterior, que 1.8 está no intervalo
# portanto, não rejeitamos H0!!!

# para testar, vamos fazer um IC unilateral para mu, com sigma desconhecido
ls = xb+qt(1-alfa,n-1)*s/sqrt(n); ls
IC95_uni_sup = c(-Inf,ls); IC95_uni_sup; round(IC95_uni_sup,2)

# vamos fazer o inferior também como teste
li = xb-qt(1-alfa,n-1)*s/sqrt(n);li
IC95_uni_inf = c(li,Inf); IC95_uni_inf; round(IC95_uni_inf,2)

# nossas conclusões batem!!!