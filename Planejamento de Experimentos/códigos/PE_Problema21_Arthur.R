library(tidyverse)
library(ExpDes.pt)

dados = read.delim("~/dados.txt")
df = tibble(dados); df

?t.test()
t.test(df$Type.1, df$Type.2, var.equal = T)

df2 = df |>  pivot_longer(cols = c(Type.1, Type.2), names_to = 'Group', values_to = 'Bt'); df2
?bartlett.test()
bartlett.test(df2$Bt, df2$Group)

tapply(df2$Bt, df2$Group, mean)
tapply(df2$Bt, df2$Group, sd)
tapply(df2$Bt, df2$Group, min)
tapply(df2$Bt, df2$Group, max)
tapply(df2$Bt, df2$Group, length)
tapply(df2$Bt, df2$Group, function(x) sd(x)/mean(x) * 100)
boxplot(df2$Bt ~ df2$Group)
points(as.factor(df2$Group), df2$Bt)

saida1 = dic(df2$Group, df2$Bt)
names(saida1)

# resíduo
plot(saida1$residuos)
abline(h=0, col = 'red')


# resíduo padronizado
plot(saida1$residuos/sqrt(86.778), ylim = c(-3,3))
abline(h = 0, col = 'red')
abline(h = 2)
abline(h = -2)
