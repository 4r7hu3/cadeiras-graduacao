#Questão 23  cap 13 do BM
X = c(23,35,29,33,43,32)
Y = c(28,38, 29,37, 42,30)
n = length(x);n #Tamanho da amostra
D = Y-X;D
D2 = D^2;D2
operario = 1:n;operario
tab = cbind(operario,  X,Y,D, D2);tab
SD = sum(D);SD
SD2 = sum(D2);SD2
#Media de D
SD/n;mean(D);mean(Y)-mean(X)
#Calculo da variancia amostral
num = SD2 - SD^2/n;num
num/(n-1)
S2D = var(D);S2D
S = sd(D);S; sqrt(S2D)
#Vamos encontra um IC para a media
#com variancia desconhecida
gama = 0.95;alfa = 1- gama; alfa
ttab =  qt(1-alfa/2,n-1);ttab;round(ttab,3)
e = ttab*(S/sqrt(n));e
DB = mean(D);DB
IC95 = DB+c(-1,1)*e;IC95
##############################################
#Direto no R
?t.test
t.test(X,Y,paired=TRUE)
t.test(Y,X,paired=TRUE)
t.test(Y,X,paired=TRUE)$conf.int
#IC gama=0.90
t.test(Y,X,paired=TRUE,conf.level=0.90)$conf.int
tcal = DB / (S/sqrt(n));tcal
nd = 2*(pt(tcal,n-1,lower.tail=FALSE));nd
2*(1-pt(tcal,n-1))
round(nd,4)
2*(1-pt(4,n-1))






