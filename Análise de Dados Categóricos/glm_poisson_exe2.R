x = c(20,35,40,50,50,60,65,65,70,90)/10; x
y = c(0,2,5,17,17,39,56,58,81,250); y

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
  mu = eta^4
  W = (16*eta^2) |> as.vector() |> diag()
  V = mu |> as.vector() |> diag()
  
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


# GLM ---------------------------------------------------------------------

link_custom = make.link("identity")
link_custom$linkfun = function(mu) mu^(1/4)
link_custom$linkinv = function(eta) eta^4
link_custom$mu.eta = function(eta) 4 * eta^3
link_custom$name = 'eta4'

fit0 = glm(y~x, family = poisson(link = link_custom))

grafico = function(x) {
  yhat = (beta[1] + beta[2]*x)**4
  plot(x,y)
  lines(x, yhat, col = 'blue', lwd=1.5)
}

grafico(x)