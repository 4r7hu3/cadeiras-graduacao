# Encontrar as raizes de x^2-5*x+6=0
f=function(x)  x^2-5*x+6
f(0)
f(2)
f(3)
plot(f,-5,5)
plot(f,-5,12.5)
plot(f,-5,10)
#?plot
abline(h=0,col='red')
abline(v=2.5,col='blue')
c=6; b=-5;a=1
polyroot(c(c,b,a))
#Escreva a densidade da N(10,4)
mu = 10
sigma = 2
f=function(x) dnorm(x,mu,sigma)
plot(f,mu-3*sigma, mu+3*sigma)
I=integrate(f,-Inf,Inf);I
I=integrate(f,-Inf,Inf)$value;I
pa=integrate(f,mu-3*sigma, mu+3*sigma)$value;pa

