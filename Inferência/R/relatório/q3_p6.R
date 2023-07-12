set.seed(508492)
library(latex2exp)

# gerando uma A.A de tamanho n=50
A = rf(50,15,15)
A = round(sort(A),3); A

# breve descrição da amostra
mean(A); var(A); summary(A)

# estimando a moda de A
estimate_mode = function(x) {
  d = density(x)
  d$x[which.max(d$y)]
}

estimate_mode(A)

# a) P(X<=2.4)
round(pf(2.4,15,15),3)
# b) P(X<=x) = 0.05
round(qf(0.05,15,15),3)
round(1/qf(0.95,15,15),3)

# função densidade de uma F com 15 e 15 g.l
f = function(x) df(x,15,15)

# gráfico do item a)
plot(f,0,4,main=TeX("$P(X \\leq 2.4) = 0.95, com\\, X \\sim\\, F(15,15)$"))
ex = c(0,seq(0,2.4,l=30),2.4)
ey = c(0,f(seq(0,2.4,l=30)),0)
polygon(ex,ey,dens=10)
axis(1,2.4)
abline(v=1,col='green')
abline(v=0.765,col='red')
abline(v=1.154,col='blue')
legend(x="topright",legend=c(TeX("$M_{o}(X) \\approx 0.765$"),TeX("$\\mu \\approx 1.154 $"),TeX("$Md(X) = 1")),col=c('red','blue','green'),lty=1)

# gráfico do item b)
plot(f,0,4,main=TeX("$P(X \\leq 0.416) \\approx 0.05, com\\, X \\sim\\, F(15,15)$"))
ex2 = c(0,seq(0,0.41,l=30),0.41)
ey2 = c(0,f(seq(0,0.41,l=30)),0)
polygon(ex2,ey2,dens=10)
axis(1,0.416)
abline(v=1,col='green')
abline(v=0.765,col='red')
abline(v=1.154,col='blue')
legend(x="topright",legend=c(TeX("$M_{o}(X) \\approx 0.765$"),TeX("$\\mu \\approx 1.154 $"),TeX("$Md(X) = 1")),col=c('red','blue','green'),lty=1)

# histograma e distribuição amostral
hist(A,prob=T,ylim=c(0,0.95),main=TeX("$Histograma\\, de\\, A \\sim\\, F(15,15)$"),ylab="Densidade")
curve(f,add=T,col='red')
lines(density(A),col='blue')
legend(x="topright",legend = c("População", "Amostra"),col=c('red','blue'),lty=1)