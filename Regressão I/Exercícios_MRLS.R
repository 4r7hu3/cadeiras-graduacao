### 10.4 ###

# dados da questão
y = c(10,11,11,13,10,11,10,11,4,2,7,10,9,9,6,5,5,5,6,4,3,3,4,10,6,8,2,0)
x = c(2205,2096,1847,1903,1457,1848,1564,1821,2577,2476,1984,1917,1761,1709,1901,2288,2072,2861,2411,2289,2203,2592,2053,1979,2048,1786,2876,2560)

dataset = data.frame(x,y); dataset

# MRLS
reg_mod = lm(y ~ x)
summary(reg_mod)

# modelo ajustado
mod_adj = function(x){return(reg_mod$coe[1] + reg_mod$coe[2]*x)}

# a)
plot(x,y)
prev = mod_adj(x)
abline(reg_mod, col='grey', lwd=3)
points(x, prev, col='blue', pch=10, lwd=3)


# b)
mod_adj(1800)


# c)
reg_mod$coe[2]*(-100)


# d)
1/reg_mod$coe[2]



# e)
yi = dataset[dataset$x == 1917,]$y; yi
ajus = mod_adj(1917); ajus
res = yi - ajus; res



### 10.5 ###

# dados da questão
y = c(259,295,279,259,299,299,309,289,359,315,310,309,300,369,419,405,439,375,379,445,379,389,369,458)/10
x = c(49176,50208,45429,45573,50597,38910,58980,56039,58282,53003,62712,59592,50500,82464,66969,77841,90384,59894,75422,87951,60831,83607,81400,91416)/10000

# a)
reg_mod = lm(y ~ x)
summary(reg_mod)


# b)
mod_adj = function(x){return(reg_mod$coe[1] + reg_mod$coe[2]*x)}
yi = mod_adj(7.5); yi


# c)
yi2 = mod_adj(5.8980); yi2
res = 30.9-yi2; res


# d)
sumario = data.frame(x,y,mod_adj(x),y-mod_adj(x))
colnames(sumario) = c('x', 'y', 'y_est', 'ei'); sumario

plot(sumario$y_est,sumario$y)
# se a linha fosse determinística, os pontos estariam sobrepostos e não haveria dispersão

plot(reg_mod)
shapiro.test(sumario$ei)

# há uma boa indicação, mas apenas a análise gráfica é insuficiente
# é preciso analisar usando métodos numéricos também



### 10.7 ###

# dados da questão
y = c(30,19,29,32,30,24,30,28,31,25,27,33,30,23,24,29,26,29,37,29)
x = c(97,209,173,121,151,156,135,181,114,302,153,90,119,80,159,97,181,173,122,173)

# a)
reg_mod = lm(y ~ x); summary(reg_mod)

# b)
m = function(x){reg_mod$coe[1] + reg_mod$coe[2]*x}
m(150)

# c)
m(114); 31-m(114)



### 10.8 ###

# dados da questão
y = c(40,42,49,46,44,48,46,43,53,52,54,57,58)
x = c(825,830,890,895,890,910,915,960,990,1010,1012,1030,1050)

# a)
reg_mod = lm(y ~ x); summary(reg_mod)
plot(x,y)
abline(reg_mod, col='blue')
points(x,m(x),col='red')

# b)
m = function(x){reg_mod$coe[1] + reg_mod$coe[2]*x}
m(910); 48-m(910)

# c)
m(950)


### 10.9 ###

# dados da questão
x = c(60,63,65,70,70,70,80,80,80,80,85,89,90,90,90,90,94,100,100,100)
y = c(1,0,1,2,5,1,4,6,2,3,5,4,6,8,4,5,7,9,7,6)

# a)
plot(x,y)
# sim, apesar de que alguns pontos aparentam ser outliers

# b)
reg_mod = lm(y ~ x); summary(reg_mod)
m = function(x){reg_mod$coe[1] + reg_mod$coe[2]*x}

# c)
m(85)


### 10.10 ###


