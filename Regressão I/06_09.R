# dados
temp = c(21,24,32,47,50,59,68,74,62,50,41,30)
cons = c(18579,21447,28803,42484,45458,53903,62155,67506,56203,45293,36995,27398)/100

n = length(temp); n

# estatísticas
Sy = sum(cons); Sx = sum(temp); Sx; Sy
Sxy = sum(temp*cons); Sxy
Sx2 = sum(temp**2); Sx2
Sy2 = sum(cons**2); Sy2

xb = Sx/n; xb
yb = Sy/n; yb

# coeficientes
beta1 = (Sxy - n*xb*yb)/(Sx2 - n*xb^2); beta1
beta0 = yb - beta1*xb; beta0


# modelo
m = lm(cons ~ temp)
summary(m)


# modelo ajustado - função explícita
mod_adj = function(x){return(beta0 + beta1*x)}
prev = mod_adj(temp)


# gráfico
plot(temp, cons)
abline(m, col='blue', lwd=3)
points(temp, prev, pch=19, col='orange')


# estimativa de 55ºF
mod_adj(55)


# estimativa de 47ºF
yi = mod_adj(47); yi

data = data.frame(temp,cons,prev,ei = cons-prev); data
data[data$temp == 47,]$cons

res = data[data$temp == 47,]$cons - yi; res

# c/5 = (f-32)/9; c = (f-32)*5/9
xc = (temp-32)*5/9; xc

plot(xc,cons)
nm = lm(cons ~ xc); summary(nm)

# centralização
xm = temp - mean(temp)

nm2 = lm(cons ~ xm); summary(nm2)

# observe que Beta1* = Beta1_est
# e que Beta0* = Beta0_est + Beta1_est * xb



