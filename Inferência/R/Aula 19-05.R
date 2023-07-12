#Uso da tabela F
#############################################

#Qual o percentil 95 da U~F(10,20)
P95 = qf(0.95, 10,20); P95; round(P95,2)#f2
P05 = qf(0.05, 10,20); P05; round(P05,2)#f1

#Como achar na tabela do bussab
a = qf(0.95,20,10);a
f1 = 1/a;f1;P05;1/2.77

#Grafico da densidade de U
f = function(u) df(u,10,20)
f(10)
f(2)
f(5)
f(7)
plot(f,0,7)

#X~N(10,16), Y~N(15,25)
#Tire uma amostra aleatoria de tamanho n = 11 de X
n = 11; mu1 = 10; sigma1 = 4 
set.seed(514096)
X = rnorm(n, mu1, sigma1);X
XB = mean(X);XB
S21 = var(X);S21
S1 = sd(X);S1

#Tire uma amostra aleatoria de tamanho n = 21 de Y
m = 21; mu2 = 15; sigma2 = 5 
set.seed(514096)
Y = rnorm(m, mu2, sigma2);Y
YB = mean(Y);YB
S22 = var(Y);S22
S2 = sd(Y);S2

f2 = P95;f2
f2 = 2.35

f1 = 1/2.77;f1
f1 = 0.36

teta_est = S22/S21;teta_est 
teta_est<4 #Regra pratica as variancias podem ser
#consideradas iguais
ls = f2*teta_est;ls
li = f1*teta_est;li

IC90 = c(li,ls);IC90;round(IC90,2)

#IC(mu1-mu2, 95%), X e Y independentes
gl = n+m-2;gl
gama =  0.95; alfa = 1 - gama;alfa;alfa/2
ttab = qt(1-alfa/2,gl);ttab;round(ttab,3)

s2p = ((n-1)*S21 + (m-1)*S22)/(n+m-2);s2p
S21;S22
sp = sqrt(s2p);sp

e = ttab * sp * sqrt(1/n+1/m);e
IC95 = (XB - YB)+c(-1,1)*e;IC95;round(IC95,2)

#Direto no R
?t.test
mod = t.test(X,Y, var.equal = TRUE);mod
names(mod)
mod
mod$statistic
mod$parameter
mod$p.value
mod$stderr

#Variancias diferentes
mod2 = t.test(X,Y);mod2
?var.test

var.test(X,Y)
var.test(Y,X)

e
sp*sqrt(1/n+1/m)

#Questão 2
n = 100; XB = 18.4; S = 5.2;  gama = 0.99
#Primeira solução pela normal, pois n=100 é grande
sigma = 5.2
alfa = 1- gama;alfa;alfa/2
ztab = qnorm(1-alfa/2);ztab;round(ztab,2)
ztab = 2.58
e = ztab *sigma/sqrt(n);e

IC99 = XB +c(-1,1)*e;IC99;round(IC99,2)
#Qual o erro padrao de X barra 
epm = sigma/sqrt(n);epm

n2 = 100*(e^2/9);n2

#Questão 3
Li = seq(1000,3500,500);Li
Ls = seq(1500,4000,500);Ls
x = (Li + Ls)/2;x
f = c(2,5,12,17,3,1)
xf = x*f;xf
x2f = x^2*f;x2f
n = sum(f);n
tab = cbind(Li,Ls, x,f,xf,x2f);tab
n>30#Vamos aproximar pela Normal
sx = sum(xf);sx
sx2 = sum(x2f);sx2
#Estimativa pontual da media
xb = sx /n;xb 
s2 = (sx2-(sx)^2/n)/(n-1);s2
s = sqrt(s2)
ttab = qt(0.95,39);ttab
ztab = qnorm(0.95);ztab
IC90t = xb + c(-1,1)*ttab*s/sqrt(n);IC90t 
IC90n = xb + c(-1,1)*ztab*s/sqrt(n);IC90n 













