set.seed(508492)
library(latex2exp)

# gerando uma A.A de tamanho n=50
A = rt(50,21)
A = round(sort(A),3);A

# breve descrição da amostra
mean(A); var(A); summary(A)

# estimando a moda de A
estimate_mode = function(x) {
  d = density(x)
  d$x[which.max(d$y)]
}

estimate_mode(A)

# a) P(X<=0.859)
round(pt(0.859,21),3)
# b) P(X<x) = 0.6
round(qt(0.6,21),3)

# função densidade de uma t de Student com 21 g.l
f = function(x) dt(x,21)

# gráfico do item a)
plot(f,-3,3,main=TeX("$P(X \\leq 0.859) = 0.8, com\\, X \\sim\\, t(21)$"))
ex = c(-3,seq(-3,0.859,l=30),0.859)
ey = c(0,f(seq(-3,0.859,l=30)),0)
polygon(ex,ey,dens=10)
abline(v=0,col='red')
legend("topleft",legend=TeX("$\\mu=M_{o}(X)=Med(X)=0$"),lty=1,col='red',cex=0.8)

# gráfico do item b)
plot(f,-3,3,main=TeX("$P(X \\leq 0.257) \\approx 0.6, com\\, X \\sim\\, t(21)$"))
ex2 = c(-3,seq(-3,0.257,l=30),0.257)
ey2 = c(0,f(seq(-3,0.257,l=30)),0)
polygon(ex2,ey2,dens=10)
abline(v=0,col='red')
legend("topleft",legend=TeX("$\\mu=M_{o}(X)=Med(X)=0$"),lty=1,col='red',cex=0.8)

# vamos plotar a distribuição da nossa A.A
hist(A,prob=T,main=TeX("$Histograma\\, de\\, A \\sim\\, t(21)$"),ylab="Densidade")
curve(f,add=T,col='red')
lines(density(A),col='blue')
legend(x="topright",legend = c("População", "Amostra"),col=c('red','blue'),lty=1)