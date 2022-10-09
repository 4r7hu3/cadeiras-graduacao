

## Lista 1 - Questão 4 - Comportamento da FDA (geral e por partes) de X

Fx<-function(x){return(0*(x< -2)+(.25+(1/8)*(x+2))*(x>=-2 & x<0)+(.75+.25*(1-exp(-x)))*(x>=0))}

#Fx(4)

curve(Fx(x),from = -3, to=3,lwd=2)

Fac<-function(x){return(0*(x< -2)+((1/4)*(x+2))*(x>=-2 & x<0)+((1-.5*exp(-x)))*(x>=0))}

curve(Fac(x),from = -3, to=3,lwd=2)

Fd<-function(x){return(0*(x< -2)+.5*(x>=-2 & x<0)+(x>=0))}

curve(Fd(x),from = -3, to=3,lwd=2)


## Lista 1 - Questão 18 - Comportamento da fdp e fda de X

fx<-function(x){return((10/x^2)*(x>10)+0*(x<=10))}

fx(20)

curve(fx(x),from=0,to=100)

Fx<-function(x){return((1-(10/x))*(x>10)+0*(x<=10))}

curve(Fx(x),from=0,to=100)









