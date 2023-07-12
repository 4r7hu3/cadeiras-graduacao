# exemplo 13.12 - BUSSAB, seção 13.5
# dados de X
n = 400
somaX = 168
p1_est = somaX/n; p1_est

# dados de Y
m = 600
somaY = 180
p2_est = somaY/m; p2_est

delta = p1_est - p2_est; delta
aux = p1_est*(1-p1_est)/n + p2_est*(1-p2_est)/m; aux
e = qnorm(1-0.05/2)*sqrt(aux); e; round(e,2)
IC95 = delta+c(-1,1)*e; round(IC95, 3)
prop.test(c(168,180), c(400,600))
prop.test(c(168,180), c(400,600), correct = FALSE)