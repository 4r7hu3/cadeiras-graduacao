

### Exemplo de Quadrado Latino, sexta-feira, 20/12/2024 
### Aula remota
### Exemplo do pdf "Aula Exercicio - Quadrado Latino - 20-12-2024"

y=c(900,800,600,500,800,700,600,1000,750,
700,900,900,550,900,850,800); y

trat=factor(c(
"A", "B", "C", "D","B", "C","D","A",
"C", "D", "A", "B","D", "A","B","C" )); trat


coluna=factor(rep(c(1,2,3,4), each=4)); coluna

linha=factor(rep(c(1,2,3,4),4)); linha

cbind(linha, coluna, trat, y)

plot(y~trat)
points(trat, y, pch=19)

require(ExpDes.pt)

bartlett.test(y~trat)


### Análise via Quadrado Latino:

saida=dql(trat,linha,coluna,y,
    quali = TRUE, mcomp = "tukey")

names(saida)
plot(saida$residuos/sqrt(3698),ylim=c(-2,2), pch=19, col=trat)
abline(h=-2, col="blue")
abline(h=0, col="blue")
abline(h=2, col="blue")

### CONTRASTES ORTOGONAIS: até 3 contrastes

a=4 ## número de trats
QMRes=3698
  
## Contraste 1:

C1=c(3, -1, -1, -1)
total=tapply(y, trat, sum); total

SQC1=(sum((C1*total))^2)/(a*sum(C1^2)) ## Soma de Quadrados do Contraste
SQC1

FC1=SQC1/QMRes; FC1
pvalor_C1=pf(FC1, 1, (a-1)*(a-2), lower.tail = FALSE);pvalor_C1
## conclusão: Rejeita-se Ho

## Contraste 2:
C2=c(0, 2, -1, -1)
total=tapply(y, trat, sum);total

SQC2=(sum((C2*total))^2)/(a*sum(C2^2)) ## Soma de Quadrados do Contraste
SQC2

FC2=SQC2/QMRes; FC2
pvalor_C2=pf(FC2, 1, (a-1)*(a-2), lower.tail = FALSE);pvalor_C2
## conclusão: Rejeita-se Ho

## Contraste 3:

C3=c(0, 0, 1, -1)
total=tapply(y, trat, sum);total

SQC3=(sum((C3*total))^2)/(a*sum(C3^2)) ## Soma de Quadrados do Contraste
SQC3

FC3=SQC3/QMRes; FC3
pvalor_C3=pf(FC3, 1, (a-1)*(a-2), lower.tail = FALSE);pvalor_C3
## conclusão: Rejeita-se Ho


### Na forma matricial:

C=rbind(C1, C2, C3); C

SQC=((C%*%total)^2)/(a*(C^2)%*%c(1,1,1,1)); SQC
FContrastes=SQC/QMRes; FContrastes

p_valores_Contrastes=pf(FContrastes, 1, (a-1)*(a-2), 
                        lower.tail = FALSE);p_valores_Contrastes

cbind(FContrastes, p_valores_Contrastes)


### Fazendo a análise via função aov(): MESMO RESULTADO!!!!

saida_aov=aov(y~trat+linha+coluna)
anova(saida_aov)
names(saida_aov)
dql(trat = trat, linha = linha, coluna = coluna, resp = y)
