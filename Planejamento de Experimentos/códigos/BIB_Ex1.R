library(tidyverse)
library(agricolae)

# Manual ------------------------------------------------------------------

bloco = 1:10; bloco

df = data.frame(bloco,
                   trat1 = c(7,6.5,6,6.8,6.8,7,rep(NA,4)),
                   trat2 = c(8,8.3,8.2,rep(NA,3),7.9,8,8.1,NA),
                   trat3 = c(10,NA,NA,9.8,9.7,NA,9.9,10,NA,9.5),
                   trat4 = c(NA,8.5,NA,8,NA,7.9,7.8,NA,7,8.5),
                   trat5 = c(NA,NA,6.9,NA,6.7,6.6,NA,7,6.9,6)); df


total_blocos = df[,-1] |> rowSums(na.rm = T); total_blocos
total_trats = df[,-1] |> colSums(na.rm = T); total_trats

k = 3
a = 5
b = choose(a, k)
N = b*k
r = colSums(!is.na(df[,-1]))[1] |> as.vector()
lambda = r*(k-1)/(a-1)

df.pl = df |> pivot_longer(cols = trat1:trat5, names_to = 'tratamento', values_to = 'y'); df.pl
df.pl = df.pl |> na.exclude(); df.pl

SQTot = sum(df.pl$y^2) - (sum(df.pl$y)^2)/N; SQTot


# Agricolae ---------------------------------------------------------------

BIB.test(block = df.pl$bloco,
         trt = df.pl$tratamento,
         y = df.pl$y,
         console = T)

