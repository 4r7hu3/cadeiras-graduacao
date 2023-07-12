set.seed(508492)
library(latex2exp)

# gerando uma A.A de tamanho n=50
A = rchisq(50,15)
A = round(sort(A),3);A

# breve descrição da amostra
mean(A); var(A); summary(A)

# estimando a moda de A
estimate_mode = function(x) {
  d = density(x)
  d$x[which.max(d$y)]
}

estimate_mode(A)

# a) P(X<11.721)
round(pchisq(11.721,15),3)
# b) P(X<x) = 0.5
round(qchisq(0.5,15),3)

# podemos ver que a moda é 13
dchisq(13,15)<dchisq(12,15)
dchisq(13,15)<dchisq(15,15)

# função densidade de uma Chi-Quadrado com 15 g.l
f = function(x) dchisq(x,15)

# gráfico do item a)
plot(f,0,40,main=TeX("$P(X \\leq 11.721) \\approx 0.3, com\\, X \\sim\\, \\chi^2_{15} $"))
ex=c(0,seq(0,11.721,l=30),11.721)
ey=c(0,f(seq(0,11.721,l=30)),0)
polygon(ex,ey,dens=10)
abline(v=13,col='red')
abline(v=15,col='blue')
abline(v=14.339,col='green')
axis(1,13)
axis(1,15)
legend(x="topright",legend=c(TeX("$M_{o}(X)=13$"),TeX("$\\mu = 15$"),TeX("$Md(X) \\approx 14.339$")),col=c('red','blue','green'),lty=1)

# gráfico do item b)
plot(f,0,40,main=TeX("$P(X \\leq 14.339) = 0.5, com\\, X \\sim\\, \\chi^2_{15} $"))
ex2 = c(0,seq(0,14.339,l=30),14.339)
ey2 = c(0,f(seq(0,14.339,l=30)),0)
polygon(ex2,ey2,dens=10)
abline(v=13,col='red')
abline(v=15,col='blue')
axis(1,14.339)
legend(x='topright',legend=c(TeX("$M_{o}(X)=13$"),TeX("$\\mu = 15$")),col=c('red','blue'),lty=1)

# vamos plotar a distribuição da nossa A.A
hist(A,prob=T,ylim=c(0,0.1),main=TeX("$Histograma\\, de\\, A \\sim\\, \\chi^2_{15}$"),ylab="Densidade")
curve(f,add=T,col='red')
lines(density(A),col='blue')
legend(x="topright",legend=c("População", "Amostra"),col=c('red','blue'),lty=1)