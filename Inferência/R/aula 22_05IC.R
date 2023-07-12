
#####Exercício 1-Gere uma amostra de tamanho 25 de X~N(mu=10,sigma2=4)  






n=25

set.seed(32)
X=rnorm(n,10,2);X


####item a. Estime  a média e  o desvio padrão

xb=mean(X);s=sd(X);xb;s


####item b. Construa um IC unilateral superior de 95% para mu considerando sigma conhecido.



gama=0.95;alfa=1-gama;alfa
z_tab=qnorm(gama);z_tab
z_tab=1.645
sigma=2
ls=xb+z_tab*(sigma/sqrt(n));ls


ICUS95=c(-Inf,ls);ICUS95


####item c. Construa um IC unilateral inferior de 95% para mu considerando sigma conhecido.


li=xb-z_tab*(sigma/sqrt(n));li


ICUI95=c(li,Inf);ICUI95




#### item d.Construa um IC unilateral superior de 95% para mu considerando sigma desconhecido.



gama=0.95;alfa=1-gama;alfa
t_tab=qt(gama,n-1);t_tab;round(t_tab,3)
ls=xb+t_tab*(s/sqrt(n));ls


ICUS95=c(-Inf,ls);ICUS95


####item e.Construa um IC unilateral inferior de 95% para mu considerando sigma desconhecido.


li=xb-t_tab*(s/sqrt(n));li


ICUI95=c(li,Inf);ICUI95






#### f: Obter direto no R:

#####Inferior

t.test(X,conf.level=0.95,alternative="less")$conf.int


#####Superior 

t.test(X,conf.level=0.95,alternative="greater")$conf.int


####gama=0.90


#####Inferior

t.test(X,conf.level=0.90,alternative="less")$conf.int


#####Superior 

t.test(X,conf.level=0.90,alternative="greater")$conf.int



t_tab=qt(1-alfa,n-1);t_tab;round(t_tab,3)


ls=xb +t_tab*s/sqrt(n);ls




#####item g-IC bilateral



t.test(X)$conf.int



t.test(X,conf.level=0.90)$conf.int #####Comentar os unilaterais!!!!




