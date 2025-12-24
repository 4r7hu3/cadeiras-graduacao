

##### Exemplos de Problemas do MOntgomery - 
##### DIC: Problema 19 - 7a Edição


y=c(80,83,83,85,75,75,79,79,74,73,76,77,67,72,
    74,74,62,62,67,69,60,61,64,66)

Diametro=c(  0.37,  0.37,  0.37,  0.37,  0.51,  
  0.51,  0.51,  0.51,  0.71,  0.71,  0.71,  0.71,  
  1.02,  1.02,  1.02,  1.02,  1.40,  1.40,  1.40,  
  1.40,  1.99,  1.99,  1.99,  1.99)

cbind(Diametro, y)
Diametro_f=factor(Diametro)

plot(y~Diametro, pch=19)

plot(y~Diametro_f)
points(Diametro_f,y, pch=19)

library(ExpDes.pt)

saida=dic(Diametro_f,y, mcomp = "tukey",hvar = "bartlett")
names(saida)

media=tapply(y, Diametro, mean)
media

## Contraste1:

n=4
C1=c(1,-3,2,0,0,0)

SQC1= n*sum(C1*media)^2 / sum(C1*C1)
SQC1
FC1=SQC1/ 7.347
FC1

pf(FC1, 1, saida$gl.residual, lower.tail=F) ## Não Rej. Ho
