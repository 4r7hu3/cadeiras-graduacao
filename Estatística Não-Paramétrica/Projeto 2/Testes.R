library(tidyverse)
library(DescTools)
library(xtable)

getwd()
setwd('~/Documentos/UFC/Estatística Não-Paramétrica/Projeto 2')

df = read.csv2('dados_projeto.csv', col.names = c('Jogador', 'P'), sep = ','); View(df)
df$P = as.numeric(df$P)

df = df %>% mutate(Sucesso = round(P*112))

hist(df$P)
shapiro.test(df$P)

hist(df$Sucesso)
shapiro.test(df$Sucesso)

# teste t

## proporção de vitórias
t.test(df$P, mu = 0.5)
t.test(df$P, mu = 0.5, alternative = "less")

## nº absoluto de vitórias
t.test(df$Sucessos, mu = 56)
t.test(df$Sucessos, mu = 56, alternative = "less")

# teste de wilcoxon
wilcox.test(df$P, mu = 0.5, conf.int = .95, alternative = "two.sided")
wilcox.test(df$P, mu = 0.5, conf.int = .95, alternative = "less")
wilcox.test(df$P, mu = 0.5, conf.int = .95, alternative = "greater")

# teste binomial
tab = data.frame(valor_p = NA, li = NA, ls = NA)
for(i in 1:length(df$Sucesso)){
  b = prop.test(df$Sucesso[i], 112, p=0.5)
  l = c(round(b$p.value,4), round(b$conf.int[1],5), round(b$conf.int[2],5))
  tab = rbind(tab, l)
}

tab = tab[-1,]; tab
row.names(tab) = NULL; tab
xtable(tab)
