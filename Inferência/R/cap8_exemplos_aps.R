# exemplo1, X ~ N(100,100)
fx = function(x){return((1/sqrt(2*pi*100))*exp((-1/200)*(x-100)^2))}

# prova da f.d.p
integrate(fx,-Inf,Inf)$value # integral no suporte = 1
plot(fx,70,130) # fx>=0 para todo x


# p(x<95)
x = 70:130
integrate(fx,-Inf,95)$value
plot(fx,70,130)
cx = c(70,70,x[x<95],95,95)
cy = c(0,fx(70),fx(x[x<95]),fx(95),0)
polygon(cx,cy,dens=10)
text(88,0.005,expression(p(X<95)),col='red')


# exemplo2, Y ~ Exp(500)
fy = function(y){return(0*(y<0)+1/500*exp(-1/500*y)*(y>=0))}

# prova que é f.d.p
integrate(fy,0,Inf)$value # integral no suporte = 1
plot(fy,0,2000) # fy>=0

# p(X>=400)
integrate(fy,400,Inf)$value
plot(fy,0,2000)
y = 0:2000
dx = c(400,400,y[y>400],2000,2000)
dy = c(0,fy(400),fy(y[y>400]),fy(2000),0)
polygon(dx,dy,col='grey')
text(710,0.00019,expression(p(X>=400)),col='red',cex=1.2)

# qual a E[Y]? sabemos que é 1/lambda = 1/(1/500) = 500, mas vamos fazer analiticamente
e = function(y){y*fy(y)}
Ey = integrate(e,0,Inf)$value;Ex

# qual a Var[Y]? sabemos que é 1/lambda^2 = 250000, mas vamos fazer analiticamente
e2 = function(y){y^2*fy(y)}
Vary = integrate(e2,0,Inf)$value - Ey^2 ;Vary# Var[Y] = E[Y²] - E²[Y]