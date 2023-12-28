install.packages('nortest')
library(nortest)
dados <- c(13.9, 17.7, 17.9, 18.3, 18.5, 18.9, 19.4, 19.8, 20.2, 20.6, 21.1, 21.3, 21.7, 21.9, 22.0, 22.2, 22.7, 22.8, 23.2, 23.3, 23.4, 23.8, 24.4, 24.9)
lillie.test(dados)
n = length(dados)

Zi = (dados - mean(dados))/sd(dados); Zi
p = pnorm(Zi); p

i = 1:n
D_plus = max(i/n - p[i]); D_plus
D_minus = max(p[i] - (i-1)/n); D_minus

D = max(D_minus,D_plus); D; round(D,4)



