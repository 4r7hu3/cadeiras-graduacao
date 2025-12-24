library(tidyverse)

# Exemplo univariado ------------------------------------------------------

df = read.csv2("~/df.txt", sep="") |> tibble(); df

df = df |> mutate(d = Depois - Antes); df
db = mean(df$d); db
s = var(df$d); s

tcalc = db/sqrt(s/8); tcalc
ttab = qt(1-0.05/2, 7); ttab

abs(tcalc) > ttab

db + c(-1,1)*ttab*sqrt(s/8)

# Exemplo multivariado ----------------------------------------------------

y = matrix(c(73,31,43,19,47,22,53,26,58,36,47,30,52,29,38,36,61,34,56,33,56,19,34,19,55,26,65,15,75,18), ncol=2, byrow = T); y
x = matrix(c(51,35,41,14,43,19,41,29,47,34,32,26,24,19,43,37,53,24,52,27,57,14,44,19,57,30,40,7,68,13), ncol=2, byrow = T); x
d = x-y; d
db = matrix(c(mean(d[,1]), mean(d[,2]))); db
Sd = cov(d); Sd


T2 = 15 * t(db) %*% solve(Sd) %*% db; T2
Fcalc = (15-2)/((15-1)*2) * T2; Fcalc
Ftab = qf(1-0.05, 2, 15-2); Ftab
Fcalc > Ftab
pf(Fcalc, 2, 15-2, lower.tail = F)