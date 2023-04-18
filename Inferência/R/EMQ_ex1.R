X = c(12, 15, 17, 20, 26)/10
Y = c(39, 47, 56, 58, 70)/10

# estimativa do coeficiente
cf = sum(X*Y)/sum(X^2);cf

# estimativa dos valores de Y
Y_est = cf*X;

# calculando o erro
err = Y - Y_est
err2 = err^2
sum(err2)

# visualizando em matriz vs data frame
tabela0 = cbind(X, Y, Y_est, err, err2);tabela0
df = data.frame(tabela0)
round(sum(df['err2']), 2)
df
attach(df)
nr = round(c(sum(X), sum(Y), sum(Y_est), sum(err), sum(err2)), 2)
df = rbind(df, nr);df
rbind(tabela0, nr)