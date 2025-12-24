
### Transformação BOX-COX #####

### mODELO DIC:

### eXEMPLO1:
trat=factor(rep(c(1,2,3,4), each=6))

y=c(
0.34,0.12,1.23,0.70,1.75,0.12,
0.91,2.94,2.14,2.36,2.86,4.55,
6.31,8.37,9.75,6.09,9.82,7.24,
17.15,11.82,10.95,17.20,14.35,16.82
)
head(cbind(trat, y))

plot(y~trat, pch=19)
points(trat, y, pch=19)

require(ExpDes.pt)

saida_0=dic(trat, y, quali=TRUE, hvar="bartlett")

require(MASS)
boxcox(y~trat, plotit=T)
saida_lam=boxcox(y~trat, lam=seq(-1,1,1/10))
names(boxcox(y~trat, plotit=T))

cbind(saida_lam$x,saida_lam$y )
dim(data.frame(saida_lam))
lambda=as.data.frame(saida_lam)
dim(lambda)

lambda[,1][lambda[,2]==max(lambda[,2])]

lambda[lambda$y==max(lambda$y),]  ### MAIS FÁCIL!!


##########################################

lb=0.5353

yt=((y^lb)-1)/lb
yt
saida_1=dic(trat, yt, quali=TRUE, hvar="bartlett")



############################################
############################################
######## OUTRO EXEMPLO - DIC:
  
sistema=factor(rep(c(1,2,3,4,5), each=6))

y=c(
2370, 1687, 2592, 2283, 2910, 3020,
1282, 1527, 871, 1025, 825, 920,
562, 321, 636, 317, 485, 842,
173, 127, 132, 150, 129, 227,
193, 71, 82, 62, 96, 44
)

hist(y)  
plot(y~sistema, pch=19)
points(sistema, y, pch=19)
  
saida_0=dic(sistema, y, quali=TRUE, hvar="bartlett")


### 1a transformação:

yt=sqrt(y)
saida_1=dic(sistema, yt, quali=TRUE, hvar="bartlett")



### 2a transformação:

yt=log(y)

saida_2=dic(sistema, yt, quali=TRUE, hvar="bartlett")
