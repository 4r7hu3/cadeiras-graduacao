x = c(20,35,40,50,50,60,65,65,70,90)/10; x
y = c(1,2,3,5,5,7,9,9,12,33); y

X = model.matrix(~x); X
Y = matrix(y); Y

plot(x,y)

et = 0.0001; et
b_0 = matrix(c(0,1)); b_0
passo = 1
N = 100

for(i in 1:N){
  cat('------------------------- PASSO', passo, '-----------------------------\n')
  
  eta = X %*% b_0
  mu = exp(eta)
  W = mu |> as.vector() |> diag()
  V = W
  
  Z = eta + solve(W^0.5) %*% solve(V^0.5) %*% (Y - mu)
  
  beta = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Z
  
  erro = beta - b_0
  
  cat('\nEstimativa:\n')
  print(t(beta))
  cat('\nErro\n')
  print(t(erro))
  cat('\n')
  
  if(sum(abs(erro) <= et) == 2) {
    cat('CONVERGIU!!!')
    break
  }
  
  b_0 = beta
  passo = passo + 1
}

fit0 = glm(y~x, family = poisson()); fit0$coe

cbind(exp(X %*% beta), Y, Y-exp(X %*% beta))
sum((Y-exp(X %*% beta))^2)

grafico = function(x) {
  yhat = exp(beta[1] + beta[2]*x)
  plot(x,y)
  lines(x, yhat, col = 'blue', lwd=1.5)
}

grafico(x)
