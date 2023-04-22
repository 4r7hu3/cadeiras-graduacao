# montagem da função
fx = function(x){return(0*(x<0 | x>6)+(x+1)/10*(x>=0 & x<=2)+(-3*x/40+9/20)*(x>2 & x<=6))}

# vamos provar que fx é uma f.d.p
plot(fx,-1,7) # fx>=0
integrate(fx,0,6)$value # integral em A é = 1

# quartis da distribuição
h = function(x){abs(0.25 - integrate(fx,0,x)$value)}
q1 = optimise(h,c(0,6))$min;q1
i = function(x){abs(0.5 - integrate(fx,0,x)$value)}
q2 = optimise(i, c(0,6))$mi;q2
j = function(x){abs(0.75 - integrate(fx,0,x)$value)}
q3 = optimise(j,c(0,6))$min;q3

# p(X>4.5)
integrate(fx,4.5,6)$value
plot(fx,0,6)
polygon(x=c(4.5,seq(4.5,6,l=30,)),y=c(0,fx(seq(4.5,6, l=30))), col="grey")
text(5.04,0.02,expression(p(X>4.5)),col="blue")
abline(v=m,col="red")

# canculando a renda média
r_m = function(x){x*fx(x)}
m = integrate(r_m,0,6)$value;m
