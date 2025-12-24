
### Problema 4.4 - eição 8:
### Modelo BIC

y=c(73,
68,
74,
71,
67,
73,
67,
75,
72,
70,
75,
68,
78,
73,
68,
73,
71,
75,
75,
69)

trat= factor(rep(c(1,2,3,4),each=5)); trat
bloco=rep(c(1,2,3,4,5), 4);  bloco

cbind(y,trat, bloco)

boxplot(y~trat)
points(trat, y, pch=19)

bartlett.test(y~trat)

tapply(y, trat, var)
tapply(y, trat, mean)

require(ExpDes.pt)

saida=dbc(trat, bloco, y,
  quali = TRUE,  mcomp = "tukey")

names(saida)
cbind(y-saida$valores.ajustados, saida$residuos)  

plot(saida$residuos/sqrt(1.817), 
     ylim=c(-2.5, 2.5),
     pch=19,
     col=rep(c(3,4,5,6), each=5))  ## resíduos padronizados!!
abline(h=-2)
abline(h=2)
abline(h=0)

qqnorm(saida$residuos/sqrt(1.817))
qqline(saida$residuos/sqrt(1.817))

cbind(y,trat)


yt=log(y)
saida=dbc(trat, bloco, yt,
          quali = TRUE,  mcomp = "tukey")

qqnorm(saida$residuos/sqrt(0.0003499))
qqline(saida$residuos/sqrt(0.0003499))
