n = 20; xb = 5.78; s = 1.28
gama = 0.98; alfa = 1-gama; gama; alfa; alfa/2
ttab = qt(1-alfa/2,n-1); ttab; round(ttab,2)

# como n=20 e sigma é desconhecido, usamos a distribuição t(n-1) = t(19), pois há 19 na tabela

e = ttab*s/sqrt(n); e; round(e,2) 
IC_98 = xb+c(-1,1)*e; IC_98; round(IC_98,2) 
e_desejado = 0.2
gama2 = 0.95; alfa2 = 1-gama2; gama2; alfa2; alfa2/2
ttab2 = qt(1-alfa2/2,n-1); ttab2; round(ttab2,2)
n2 = ceiling((ttab2*s/e_desejado)^2); n2