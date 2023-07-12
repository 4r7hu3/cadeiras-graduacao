
###CC0288_Inferência Estatística I-24/04/2023

###Distribuição de Weibull

# gere uma amostra de tamanho n=100 de X ~ W(a=3,b=2)

# Vamos criar a densidade
a = 3;b=2
f = function(x) a*b*x^(b-1)*exp(-a*x^b)
f(2)
f(1)
F = function(x) 1 - exp(-a*x^b)
F(2)
F(4)
plot(f,0,3)
plot(f,0,2)
abline(h=0,col='red')
I = integrate(f,0,Inf)$value;I
b>1
mo = ((b-1)/(a*b))^(1/b);mo
abline(v=mo,col='blue')
mu = a^(-1/b)*gamma(1+1/b);mu
fx = function(x) x*f(x)
Ex = integrate(fx,0,Inf)$value;Ex
fx2 = function(x) (x^2)*f(x)
Ex2 = integrate(fx2,0,Inf)$value;Ex2
a^(-2/b)*gamma(1+2/b)
vX = Ex2 - Ex^2;vX

# crie uma função no R para calcular o q-ésimo quantil

quantil = function(q,a,b){
  x = ((-log(1-q)/a)^(1/b))
  return(x)
}

med = quantil(0.5,3,2);med
Q3 = quantil(0.75,3,2);Q3

?dweibull




####Pedir  med e q