# Ex1
## X ~ Exp(2)
f = function(x) {
  fx = ifelse(x>=0,2*exp(-2*x),0)
  return(fx)
}

# poderia ser:
# g = function(x){return(0*(x<0)+2*exp(-2*x)*(x>=0))}

# plot de fx
plot(f)
plot(f,0,10)
plot(f,0,5)

# integral em A é 1
integrate(f, 0, Inf)$value

# p(X>1)
1 - integrate(f, 0, 1)$value

# p(0.2<X<0.8)
integrate(f,0.2,0.8)$value

# Ex2
## montagem da função
f2 = function(x){return(0*(x<0 | x>=3)+2/3*x*(x>=0 & x<1)+(1-x/3)*(x>=1 & x<3))}

# testando a função
integrate(f2,0,3)$value

# plot de f2
curve(f2,-1,4)

# a) p(X>1.5)
a = integrate(f2,1.5,Inf)$value;a # mesmo que 1-integrate(f2,0,1.5)

# b) valor de venda esperado em 30 dias
# primeiro, calculampos a esperança E[X] da função
Ef2 = function(x){x*f2(x)}
b = 30*integrate(Ef2,0,3)$value;b

# calcular o quantil 0.95 da função
# em suma, é um problema de otimização numérica
h = function(x){abs(0.95 - integrate(f2,0,x)$value)}
q = optimise(h,c(0,3))$minimum;q