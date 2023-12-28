library(tidyverse)

gpA = c(161,160,128,168,131,107,219,147,175,119,190,203,107,153,128,209,142,124)
gpB = c(128,131,125,141,157,132,143,112,131,128,139,135,127,121,168)
gpC = c(121,98,81,128,91,117,136,95,105,128,90,151,129,95,112)
gpD = c(109,65,97,96,110,67,106,102,80,96)

?kruskal.test()
kruskal.test(list(gpA, gpB, gpC, gpD))

df = data.frame('GrupoA' = gpA, GrupoB = c(gpB, rep(NA, 3)), GrupoC = c(gpC, rep(NA,3)), GrupoD = c(gpD, rep(NA, 8)))
df = pivot_longer(df, cols = paste("Grupo", LETTERS[1:4], sep = ''), names_to = "Grupo", values_to = "Resposta")

df = df %>% drop_na(); sum(is.na(df))
df %>% ggplot(aes(Grupo, Resposta, color=Grupo)) + geom_boxplot()
df %>% mutate(Ranks = rank(Grupo)) %>% arrange(Ranks)
