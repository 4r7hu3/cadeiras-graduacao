n = 36; xb = 196; sig = 50
alfa = 0.05
ztab = qnorm(1-alfa/2)
e = ztab*sig/sqrt(n)
IC_95 = xb+c(-1,1)*e; round(IC_95,2)

# se nossa H0 fosse "mu = 200" e 200 estivesse dentro de IC_95, então não rejeitamos H0
# para esse caso, 200 pertence a  IC_95
# mas nossa H0 verdadeira é "mu>=200", e H1: mu<200
# precisamos construir um IC unilateral para mu

ls = xb+qnorm(1-alfa)*sig/sqrt(n); ls
IC = c(-Inf,ls); IC
# como 200 pertence ao intervalo unilateral construído, logo, não rejeitamos H0!!!