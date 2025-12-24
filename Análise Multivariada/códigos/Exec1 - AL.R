#### libs ####
library(MASS)
library(matrixcalc)


#### Decomposição L.U. ####

## Exemplo 1 ##

A = matrix(c(3,2,4,1,1,2,4,3,2), nrow=3, byrow=T); A
U = matrix(c(3,2,4,0,1/3,2/3,0,0,-4), nrow=3, byrow=T); fractions(U)
L = matrix(c(1,0,0,1/3,1,0,4/3,1,1), nrow=3, byrow=T); fractions(L)    

L %*% U

# lu.decomposition(A)    

## Exemplo 2 ##

A = matrix(c(2,1,1,0,4,3,3,1,8,7,9,5,6,7,9,8), nrow=4, byrow=T); A
U = matrix(c(2,1,1,0,0,1,1,1,0,0,2,2,0,0,0,2), nrow=4, byrow=T); fractions(U)
L = matrix(c(1,0,0,0,2,1,0,0,4,3,1,0,3,4,1,1), nrow=4, byrow=T); fractions(L)

L %*% U

# lu.decomposition(A)


#### Decomposição Espectral ####

## Exemplo 1 ##

A = matrix(c(-1,2,2,-1), nrow=2, byrow=T); A
lambda = c(1,-3)
v1 = c(1,1)
v2 = c(-1,1)
e1 = v1/sqrt(1**2 + 1**2)
e2 = v2/sqrt((-1)**2 + 1**2)
P = matrix(c(e1,e2), ncol=2); fractions(P)

P %*% diag(lambda) %*% t(P)

# eigen(A)

## Exemplo 2 ##

A = matrix(c(9,-2,-2,6), nrow=2, byrow=T); A
lambda = c(10,5)
v1 = c(-1,2)
v2 = c(1,1/2)
e1 = v1/sqrt((-1)**2 + 2**2)
e2 = v2/sqrt(1**2 + (1/2)**2)
P = matrix(c(e1,e2), ncol=2); fractions(P)

P %*% diag(lambda) %*% t(P)

# eigen(A)