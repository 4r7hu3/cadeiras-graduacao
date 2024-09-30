dados = LAVRAS[,2]

# transformando em série temporal
serie = ts(dados,start=c(1966,1),frequency = 12)
head(serie);tail(serie)
plot(serie)
decompose(serie)
plot(decompose(serie))

# conjunto de treino e de teste
treino = window(serie,end=c(1996,12))
teste = window(serie,start=c(1997,1))
plot(treino)
adf.test(treino)
acf(treino,48)
pacf(treino,48)

# diferenca sazonal
dif = diff(treino,k=12)
plot(dif)
adf.test(dif)
acf(dif,48)
pacf(dif,48)

# primeiro modelo
# SARIMA(0,0,0)x(0,1,1)
fit = Arima(treino,orde=c(0,0,0),
            seasonal=c(0,1,1),
            include.drift = T)
summary(fit)

# tira o drift pq n tem significancia
fit2 = Arima(treino,orde=c(0,0,0),
             seasonal=c(0,1,1),
             include.drift = F)
summary(fit2)
acf(fit2$residuals)
pacf(fit2$residuals)

# mais um modelo
fit3 = Arima(treino,orde=c(9,0,0),
             seasonal=c(0,1,1),
             fixed=c(NA,0,0,0,0,0,
                     0,0,NA,NA),
             include.drift = F)
summary(fit3)
acf(fit3$residuals)
pacf(fit3$residuals)

# mais um modelo
fit4 = Arima(treino,orde=c(15,0,0),
             seasonal=c(0,1,1),
             fixed=c(NA,0,0,0,0,0,
                     0,0,NA,0,0,0,
                     0,0,NA,NA),
             include.drift = F,
             method='CSS')
summary(fit4)
acf(fit4$residuals,48)
pacf(fit4$residuals,48)