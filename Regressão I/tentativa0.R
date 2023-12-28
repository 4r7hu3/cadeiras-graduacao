# semente para a geração de números
set.seed(0)

# base de dados
dados = iris

# queremos saber se há relação entre a largura da pétala e o comprimento da sépala por meio de regressão
# nossa relação é: Y = largura da pétala e X = comprimento da sépala


# modelo direto
m = lm(iris$Petal.Width ~ iris$Sepal.Length); summary(m)


# vamos encontrar QMRes do modelo acima, para usá-lo na nossa simulação:
resp = dados$Petal.Width; exp = dados$Sepal.Length
Qmres = (sum((resp - mean(resp))^2) - sum((exp - mean(exp))*resp)^2/sum((exp - mean(exp))^2))/(length(exp)-2); Qmres


# vamos construir um IC para Beta1, simulando 100 amostras de tamanho 30, com reposição,
# e Yi_est ~ N(Beta0_est+Beta1_est*Xi; Qmres*(1/n + (x - xb)/Sxx))


# gerando 100 amostras aleatórias de tamanho 30
amostras = c()
for(i in 1:100){
  amostras[i] = list(sample(dados$Sepal.Length, 30, replace = T))
}

amostras

# função para criar os estimadores beta1 e QMRes para cada amostra, dado um Yi normal com os parâmetros anteriores
calculo = function(x){
  x = unlist(x)
  xb = mean(x); Sxx = sum((x - xb)^2)
  y = rnorm(30, m$coe[1]+m$coe[2]*x, Qmres*(1/length(x) + (x - xb)^2/Sxx))
  yb = mean(y); Syy = sum((y -yb)^2); Sxy = sum((x - xb)*y)
  QMRes = (Syy - Sxy^2/Sxx)/(length(x)-2)
  beta1_est = Sxy/Sxx
  erro = sqrt(QMRes/Sxx)
  return(c(beta1_est, erro))
}

# testando para uma única amostra
calculo(amostras[1])

# t(1-alfa/2, n-2), pois a hipótese considerada é bilateral, com alfa = 5%
t = qt(1-0.05/2,28); t

# cnstruindo os intervalos
intervalos = c()
for(i in 1:100){
  intervalos[i] = list(round(calculo(amostras[i])[1]+c(-1,1)*t*calculo(amostras[i])[2], 3))
}

# pritando direto os intervalos
for(i in 1:100){print(unlist(intervalos[i]))}

int = data.frame(Li = NA, Ls = NA); int

# criando dataframe
for(i in 1:100){
  int = rbind(int, unlist(intervalos[i]))
}

int = int[-1,]
head(int, 10)

# cotando os intervalos que têm beta1
cont = 0
for(i in 1:100){
  if(0.753>=int[i,1] && 0.753<=int[i,2]){
    cont = cont+1
  }
}

cont
