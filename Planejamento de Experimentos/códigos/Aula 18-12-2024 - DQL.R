

### QUADRADO LATINO:

### exemplo do help no R:

data(ex3)
attach(ex3)

dql(ex3$trat, ex3$linha, ex3$coluna, ex3$resp, quali=TRUE)

bartlett.test(ex3$resp~ex3$trat)

plot(ex3$resp~ex3$trat)
points(ex3$trat,ex3$resp, pch=19)

table(ex3$linha,ex3$coluna)
table(ex3$linha,ex3$trat)
table(ex3$trat,ex3$coluna)
  
###############################################

### Quadrado Latino: Montgomery, 8a ed, Problema 4.22: 

#### 

y=c(8,11,4,6,4,7,2,9,8,2,1,7,10,6,3,7,3,1,6,8,3,8,5,10,8); y

## os tratamentos entram na mesma ordem que aparcem na tabela 
## no livro!!

trat=factor(c(
"A","C","B","D","E",
"B","E","A","C","D",
"D","A","C","E","B",
"C","D","E","B","A",
"E","B","D","A","C")); trat

table(trat)

dia=rep(c(1,2,3,4,5), each=5); dia
batch=rep(c(1,2,3,4,5), 5); batch

table(batch, dia)
table(batch, trat)
table(dia,trat)

cbind(batch, dia, trat, y)

saida=dql(trat,batch, dia, y,  mcomp = "tukey")

# Igual ao dql() no ExpDes.pt:
summary(aov(y~trat+factor(batch)+factor(dia))) 



