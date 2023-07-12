n = 100; xb = 18.4; s = 5.2
gama = 0.99; alfa=1-gama; gama; alfa; alfa/2

# como n=100 e sigma é desconhecido, fazemos sigma = s, e usamos a distribuição normal padrão, pois (n-1) = 99 não tem na tabela da t

ztab = qnorm(1-alfa/2); ztab; round(ztab,2)
e = ztab*s/sqrt(n); e
IC_99 = xb+c(-1,1)*e; IC_99; round(IC_99,2)
e_desejado = 3
n2 = ceiling((ztab*s/e_desejado)^2); n2