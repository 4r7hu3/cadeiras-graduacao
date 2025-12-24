

### TRANSFORMAÇÃO DE DADOS BINOMIAIS:

### pt=arcsen(raiz(y/m))

### Exemplo de dados da Apostila da Clarice, Exemplo 5, pag:16,
### Tabela 3.

## Exemplo 5: Os dados da Tabela 3 referem-se a um ensaio de toxicidade 
## de rotenone, no delineamento completamente casualizado, 
## em que doses (di) do inseticida foram aplicadas a mi insetos 
## (Macrosiphoniella sanborni, pulgão do crisântemo, Martin, 1942) e 
## após um determinado tempo foram observados os números (yi) de 
## insetos mortos


y=c(44 , 42 , 24 , 16 , 6 , 0 )

m=c(50,49,46,48,50,49)

dose=c(10.2, 7.7, 5.1,3.8, 2.6, 0) 
p=y/m

plot(p~dose, pch=19)

dosef=factor(c(10.2, 7.7, 5.1,3.8, 2.6, 0) )
plot(p~dosef, pch=19)
boxplot(p~dosef)

cbind(p, dose, dosef)

require(ExpDes.pt)

### ANOVA PARA A proporção de mortes:

anova(aov(p~dosef))

saida_p=dic(dosef, p, quali=T, hvar="bartlett")


### Adaptando os dados: inserindo repetições por tratamento:

y=c(44, 42 , 42,41, 24,25 , 16,18 , 6,8 , 0,1 )
m=c(50,50,  49,49,  46,46, 48,48,  50,50 ,49,49 )
dose=c(10.2,10.2, 7.7,7.7,  5.1,5.1, 3.8,3.8,  2.6,2.6, 0,0) 
dosef=factor(dose)

p=y/m
cbind(y, m, p, dose, dosef)

plot(p~dose, pch=19)
boxplot(p~dosef)  ## não faz setindo com 2rep/trat

saida_p=dic(dosef, p, quali=T, hvar="bartlett")

### Transformando:

pt=asin(sqrt(p))
plot(pt~dose, pch=19)

saida_pt=dic(dosef, pt, quali=T, hvar="bartlett")

cbind(p, pt, (sin(pt))^2)


## dose quantitativa:

saida_pt=dic(dose, pt, quali=F, hvar="bartlett")

### Usando o lm():

### 1o grau:

saida1=lm(pt~dose)
anova(saida1)
model.matrix(saida1)
coef(saida1)

saida2=lm(pt~dose+I(dose^2))
anova(saida2)
model.matrix(saida2)

saida3=lm(pt~dose+I(dose^2)+I(dose^3))
anova(saida3)
model.matrix(saida3)

plot(pt~dose, pch=19)
lines(spline(dose, fitted(saida1)), n=201, lwd=2, col="blue")
lines(spline(dose, fitted(saida2)), n=201, lwd=2, col="green")
lines(spline(dose, fitted(saida3)), n=201, lwd=2,col="hotpink")

## Graficos na escala original:

plot(p~dose, pch=19)
lines(spline(dose, (sin(fitted(saida1))^2 )), n=201, lwd=2, col="blue")
lines(spline(dose, (sin(fitted(saida2))^2 )), n=201, lwd=2, col="green")
lines(spline(dose, (sin(fitted(saida3))^2 )) , n=201, lwd=2,col="hotpink")


