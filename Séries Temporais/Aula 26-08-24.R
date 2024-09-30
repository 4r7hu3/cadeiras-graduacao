######################################################
install.packages('tseries')
install.packages('forecast')
# pacotes
library(tseries)
library(forecast)

# dados
dados = OZONIO[,3]

# transformando em série temporal
serie = ts(dados,start=c(1956,1),frequency = 12)
head(serie);tail(serie)
plot(serie)
decompose(serie)
plot(decompose(serie))
adf.test(serie)
acf(serie)
pacf(serie)

install.packages('TSA')
library(TSA)
x1 = harmonic(serie,1);x2 = harmonic(serie,2);x1;x2
?harmonic

reg = lm(serie~x1); summary(reg)
reg2 = lm(serie~x2); summary(reg2)

plot(reg2$fitted.values, type='l', ylim=c(0,10))
lines(as.vector(serie), col='seagreen')

residuos = reg2$residuals
plot(residuos, type='l')
acf(residuos)
pacf(residuos)

fit1 = Arima(residuos, order=c(1,0,0))
summary(fit1)
acf(fit1$residuals)
pacf(fit1$residuals)
shapiro.test(fit1$residuals)
Box.test(fit1$residuals,lag=12, type = 'Ljung-Box')

auto.arima(fit1$residuals)

######################################################