# Questão 1 ---------------------------------------------------------------

yb1 = matrix(c(-1,-1)); yb1
yb2 = matrix(c(2,1)); yb2
Sc = matrix(c(4,1,1,3), ncol = 2); Sc

a = solve(Sc) %*% (yb1 - yb2); a

z1 = t(yb1) %*% a; z1
z2 = t(yb2) %*% a; z2

pc = (z1+z2)/2; pc

x01 = matrix(c(0,0.5))
x02 = matrix(c(0.5,0))

t(x01) %*% a |> as.vector() # > pc, logo pertence ao Grupo 1
t(x02) %*% a |> as.vector() # = pc, logo pode pertencer a qualquer dos dois grupos

# Questão 3 ---------------------------------------------------------------

y1 = matrix(c(148,159,144,103,121,89,119,20,24,19,18,17,11,17), ncol = 2); y1
y2 = matrix(c(137,164,224,208,178,128,154,15,25,27,33,24,20,18), ncol = 2); y2
n = dim(y1)[1]; n
p = dim(y1)[2]; p

d = y1 - y2; d
db = matrix(c(mean(d[,1]), mean(d[,2]))); db
Sd = cov(d); Sd
Sd.inv = solve(Sd); Sd.inv

T2 = n * t(db) %*% Sd.inv %*% db; T2
F0 = (n-p)/(p * (n-1)) * T2; F0
Ftab = qf(1-0.05, p, n-p); Ftab

F0 < Ftab # não rejeitar H0
pf(F0, p, n-p, lower.tail = F) # não rejeitar H0

# Questão 4 ---------------------------------------------------------------

yb1 = matrix(c(6.3,4.3,8.8)); yb1
yb2 = matrix(c(6,4.5,10)); yb2
Sc = matrix(c(2,1,2,1,4,1,2,1,3), ncol = 3); Sc
a = 3-1
b = n1+n2-2

matplot(c(2,6,10), cbind(yb1, yb2), col = c('blue', 'red'), type = 'l')

n1 = 5
n2 = 7
cn = matrix(c(1,-1,0,0,1,-1), ncol = 3, byrow = T); cn

T2 = (n1*n2)/(n1+n2) * t(cn %*% (yb1-yb2)) %*% solve(cn %*% Sc %*% t(cn)) %*% (cn %*% (yb1-yb2)); T2
Fcalc = T2 * (b-a+1)/(b*a); Fcalc
Ftab = qf(1-0.05, a, b-a+1); Ftab
Fcalc < Ftab
pf(Fcalc, a, b-a+1, lower.tail = F)

### NÃO REJEITAMOS O PARALELISMO ENTRE OS PERFIS

j = matrix(c(1,1,1)); j
num = t(j) %*% (yb1 - yb2); num
dem = sqrt( (1/n1 + 1/n2) * t(j) %*% Sc %*% j); dem
t = num/dem; t
ttab = qt(1-0.05/2, n1+n2-2); ttab
t < ttab
pt(t, n1+n2-2, lower.tail = F)

### NÃO REJEITAMOS A HIPÓTESE DE PERFIS COM MESMO NÍVEL

yb = (n1*yb1 + n2*yb2)/(n1+n2); yb

T2 = t(cn %*% yb) %*% solve((cn %*% Sc %*% t(cn))/(n1+n2)) %*% (cn %*% yb); T2
Fcalc = T2 * (b-a+1)/(b*a); Fcalc
Ftab = qf(1-0.05, a, b-a+1); Ftab
Fcalc < Ftab
pf(Fcalc, a, b-a+1, lower.tail = F)

### REJEITAMOS A HIPÓTESE DE PERFIS COPLANARES  