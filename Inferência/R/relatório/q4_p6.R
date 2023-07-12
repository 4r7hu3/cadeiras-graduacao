set.seed(508492)
library(latex2exp)

# gerando uma A.A de tamanho n=50
A = rnorm(50,40,5)
A = round(sort(A),3); A

# breve descrição da amostra
mean(A); var(A); summary(A)

# estimando a moda de A
estimate_mode = function(x) {
  d = density(x)
  d$x[which.max(d$y)]
}

estimate_mode(A)

# a) P(|X-40|<12)
round(pnorm(40+12,40,5) - pnorm(40-12,40,5),3) # direto
round(2*pnorm(12/5)-1,3) # padronizado
# b) P(X<=x) = 0.05
round(qnorm(0.05,40,5),3)
round(40-1.645*5,3)

# função densidade de uma N(40,25)
f = function(x) dnorm(x,40,5)

# gráfico do item a), usando N(40,25)
plot(f,40-3*5,40+3*5,main=TeX("$P(|X-40| < 12) \\approx 0.984, com\\, X \\sim\\, N(40,25)$"))
ex = c(40-12,seq(40-12,40+12,l=30),40+12)
ey = c(0,f(seq(40-12,40+12,l=30)),0)
polygon(ex,ey,col='grey')
text(40,0.03,TeX("$P(28 < X < 52) = 0.984$"),col="blue")
axis(1,c(28,52))
abline(v=40,col='red')
legend("topleft",legend=TeX("$\\mu=M_{o}(X)=Med(X)=40$"),lty=1,col='red',cex=0.8)

# gráfico do item a), usando N(0,1)
g = function(x) dnorm(x)
plot(g,-3,3,main=TeX("$P(|Z| < 2.4) \\approx 0.984, com\\, Z \\sim\\, N(0,1)$"))
ex2 = c(-2.4,seq(-2.4,2.4,l=30),2.4)
ey2 = c(0,g(seq(-2.4,2.4,l=30)),0)
polygon(ex2,ey2,col='grey')
text(0,0.15,TeX("$P(-2.4 < Z < 2.4) = 0.984$"),col='blue')
axis(1,c(-2.4,2.4))
abline(v=0,col='red')
legend("topleft",legend=TeX("$\\mu=M_{o}(Z)=Med(Z)=0$"),lty=1,col='red',cex=0.8)
        
# gráfico do item b), usando N(40,25)
plot(f,40-3*5,40+3*5,main=TeX("$P(X \\leq 31.775) \\approx 0.05, com\\, X \\sim\\, N(40,25)$"))
ex3 = c(40-3*5,seq(25,31.775,l=30),31.776)
ey3 = c(0,f(seq(25,31.775,l=30)),0)
polygon(ex3,ey3,col='grey')
axis(1,31.77)
abline(v=40,col='red')
legend("topleft",legend=TeX("$\\mu=M_{o}(X)=Med(X)=40$"),lty=1,col='red',cex=0.8)

# gráfico do item b), usando N(0,1)
plot(g,-3,3,main=TeX("$P(Z \\leq -1.645) \\approx 0.05, com\\, Z \\sim\\, N(0,1)$"))
ex4 = c(-3,seq(-3,-1.645,l=30),-1.645)
ey4 = c(0,g(seq(-3,-1.645,l=30)),0)
polygon(ex4,ey4,col='grey')
axis(1,-1.645)
abline(v=0,col='red')
legend("topleft",legend=TeX("$\\mu=M_{o}(Z)=Med(Z)=0$"),lty=1,col='red',cex=0.8)

# histograma e distribuição amostral
hist(A,prob=T,main=TeX("$Histograma\\, de\\, A\\, \\sim\\, N(40,25)$"),ylim=c(0,0.08),ylab="Densidade")
curve(f,add=T,col='red')
lines(density(A),col='blue')
legend(x="topright",legend = c("População", "Amostra"),col=c('red','blue'),lty=1)