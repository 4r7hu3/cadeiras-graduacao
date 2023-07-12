# X ~ Weibull(2)
fx = function(x){return(0*(x<0)+2*x*exp(-x^2))*(x>=0)}

# provando que X é uma f.d.p
integrate(fx,0,Inf)$value
plot(fx,0,3)

# qual a E[X]?
m1x = function(x){x*fx(x)}
Ex = integrate(m1x,0,Inf)$value;Ex

# Y ~ Weibull(5)
fy = function(y){return(0*(y<0)+5*y^4*exp(-y^5)*(y>=0))}

# provando que Y é uma f.d.p
integrate(fy,0,Inf)$value
plot(fy,0,3)

# qual a E[Y]?
m1y = function(y){y*fy(y)}
Ey = integrate(m1y,0,Inf)$value;Ey

# calculando algumas probabilidades, para ambas as V.A's
# a) p(X>2) b) p(1.5<X<6) c) p(X<8)

# para X
a = integrate(fx,2,Inf)$value;a
b = integrate(fx,1.5,6)$value;b
c = integrate(fx,0,8)$value;c

# para Y
a2 = integrate(fy,2,Inf)$value;a2
b2 = integrate(fy,1.5,6)$value;b2
c2 = integrate(fy,0,8)$value;c2