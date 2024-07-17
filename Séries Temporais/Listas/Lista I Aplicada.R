library(tidyverse)
library(forecast)

# Lista I Aplicada

## Questão 1
plot.ts(rnorm(48)) # repetir várias vezes

# parece haver aleatoriedade

## Questão 2
plot.ts(rchisq(48, 2)) # repetir várias vezes

# parece haver aleatoriedade e claramente não normalidade

## Questão 3
proc = rnorm(50)
erro = rnorm(50, 0, 20)

### a)
a = function(t){100 + 10*t + erro}
plot.ts(a(proc))

### b)
b = function(t){100 - 10*t + 2*t**2 + erro}
plot.ts(b(proc))

### c)
c = function(t){100 + 10*sin(2*pi*t/12) - 20*cos(2*pi*t/12) + erro}
plot.ts(c(proc))

### d)
d = function(t){100 + 10*t + 10*t*sin(2*pi*t/12) - 20*t*cos(2*pi*t/12) + erro}
plot.ts(d(proc))

### e)
e = function(t){100 + 10*sin(2*pi*t/12) - 20*cos(2*pi*t/12) - 5*sin(2*pi*t/6) + 30*cos(2*pi*t/6) + erro}
plot.ts(e(proc))

### f)
f = numeric(50)
f1 = 100 + 0.8*proc[1] + erro[1]; f1
for(i in 2:50){
  f[i] = 100 + 0.8*f[i-1] + erro[i]
}
plot.ts(f)

### g)
g = numeric(50); g
g[1] = 100 + erro[1] + 0.8*erro[1]; g[1]
for(i in 2:50){
  g[i] = 100 + erro[i] + 0.8*erro[i-1]
}
plot.ts(g)

### h)
h = numeric(50); h
h[1] = 100 + erro[1] - 0.8*erro[1]; h[1]
for(i in 2:50){
  h[i] = 100 + erro[i] - 0.8*erro[i-1]
}
plot.ts(h)





