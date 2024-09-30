####### bibliotecas ######
# install.packages('tseries')
# install.packages('forecast')
# install.packages('tidyverse')

library(tseries)
library(forecast)
library(tidyverse)
library(patchwork)

####### env ######
getwd()
setwd('C:/Users/aluno/Documents/Arthur')


####### Análises - AirPassengers ######
AirPassengers

AirPassengers %>% autoplot()

m0 = lm(force(AirPassengers) ~ force(time(AirPassengers)))
plot(AirPassengers)
abline(m0)

AirPassengers %>% autoplot() + geom_smooth(method = 'lm')
AirPassengers %>% autoplot() + geom_smooth()

boxplot(AirPassengers ~ cycle(AirPassengers))

decompose(AirPassengers) %>% autoplot()
decompose(AirPassengers, type = 'm') %>% autoplot() 

# o teste estranhamente rejeita a H0 de não estacionariedade
adf.test(AirPassengers)

forecast::ggtsdisplay(AirPassengers)

Box.test(AirPassengers, type='Ljung-Box')

ndiffs(AirPassengers)
forecast::ggtsdisplay(diff(AirPassengers)) # primeira dif
forecast::ggtsdisplay(log(AirPassengers)) # transformação
forecast::ggtsdisplay(diff(log(AirPassengers))) # diff 1 e transformação
forecast::ggtsdisplay(diff(diff(AirPassengers, lag = 12))) # dif saz e dif 1

####### Simulações ######

# simulando passeio aleatório
set.seed(0)
Z = a = rnorm(1000)
for(t in 2:1000){
  Z[t] = Z[t-1] + a[t]
}

# plots
Z %>% plot.ts()
forecast::ggtsdisplay(Z)
forecast::ggtsdisplay(diff(Z))

adf.test(Z)
adf.test(diff(Z))

# simulando AR(1), phi = 0.8
set.seed(0)
AR = b = rnorm(1000)
for(t in 2:1000){
  AR[t] = 0.8*AR[t-1] + b[t]
}


# plots
AR %>% plot.ts()
forecast::ggtsdisplay(AR)
forecast::ggtsdisplay(diff(AR))

####### Análises - ICV ######
library(readxl)
ICV = read_excel("Séries Temporais/ICV.xls")
ICV_ts = ts(data = ICV$ICV, start = 1970, frequency = 12); ICV_ts

# plots
forecast::ggtsdisplay(ICV_ts)
forecast::ggtsdisplay(log(ICV_ts))
forecast::ggtsdisplay(diff(log(ICV_ts)))
forecast::ggtsdisplay(diff(diff(log(ICV_ts))))

# testes
adf.test(diff(log(ICV_ts)))
adf.test(diff(diff(log(ICV_ts))))

fit1 = Arima(y = log(ICV_ts), order = c(1,1,0)); summary(fit1)

fit2 = Arima(y = diff(log(ICV_ts)), order = c(1,0,1)); summary(fit2)

fit3 = auto.arima(ICV_ts); summary(fit3)

fit4 = arma(diff(log(ICV_ts)), order = c(1,0)); summary(fit4)

forecast::ggtsdisplay(diff(diff(log(ICV_ts))))

fit5 = Arima(log(ICV_ts), order = c(1,1,0), include.drift = T)

checkresiduals(fit5)
checkresiduals(fit1)
Box.test(fit3$residuals, lag=12, type = 'Ljung-Box')
Box.test(fit5$residuals, lag=20, type = 'Ljung-Box')
shapiro.test(fit5$residuals)
autoplot(fit5) # esperamos que todos os pontos caiam dentro, pois ele plot a inversa das raízes
Pacf(fit5$residuals)
Acf(diff(log(ICV_ts)))
Pacf(diff(log(ICV_ts)))
Arima(y = log(ICV_ts), order = c(2,1,0), include.drift = T) |> summary()
Arima(y = log(ICV_ts), order = c(2,1,0), include.drift = T) |> checkresiduals()
Pacf(Arima(y = log(ICV_ts), order = c(2,1,0), include.drift = T)$res)

# melhor ajuste
mb = Arima(y = log(ICV_ts), order = c(1,1,1), include.drift = T)
Arima(y = log(ICV_ts), order = c(1,1,1), include.drift = T) |> checkresiduals()
Pacf(mb$residuals)
Pacf(Arima(y = log(ICV_ts), order = c(1,1,1), include.drift = T)$res)
Arima(y = log(ICV_ts), order = c(1,1,1), include.drift = T)$res |> shapiro.test()
Arima(y = log(ICV_ts), order = c(1,1,1), include.drift = T) |> accuracy()
fit5 |> accuracy()
fit5$aic
Arima(y = log(ICV_ts), order = c(1,1,1), include.drift = T)$aic
autoplot(Arima(y = log(ICV_ts), order = c(1,1,1), include.drift = T))

# Zt - Zt-1 = (phi x Zt-1) + at (- 0.8262 x at-1) + 0.0275
# diferença      AR(1)              MA(1)             mu

#### Treino, teste, avaliação, validação ####

treino = head(ICV_ts, 114); treino
teste = tail(ICV_ts, -114); teste

mb_t = Arima(log(treino), order = c(1,1,1), include.drift = T); mb_t
mb_t_box = Arima(treino, order = c(1,1,1), include.drift = T, lambda = 0); mb_t_box

# Zt = Zt-1(1 + 0,7481) - 0,7481Zt-2 + at + 0,3418at-1 + 0,0213
# V(Zt) = 9,167e-05
# E[Zt] = 0,0213


mb_t$residuals |> ggtsdisplay()
mb_t$residuals |> adf.test()
mb_t$residuals |> kpss.test()
mb_t$residuals |> Box.test(type = 'Ljung-Box')
mb_t |> autoplot()


tibble(Coe = mb_t$coef,
       S.E. = sqrt(diag(vcov(mb_t))),
       Stat = Coe/S.E.,
       PValor =  2 * (1 - pt(abs(Stat), df = length(ICV_ts)))) # todos são significantes


mb_t_prev = forecast(mb_t, h = 12, level = c(95)); mb_t_prev
mb_t_box_prev = forecast(mb_t_box, h = 12, level = c(95)); mb_t_box_prev

mb_t_prev |> autoplot()

log(ICV_ts) |> autoplot() + autolayer(mb_t_prev)

accuracy(mb_t_prev, x = teste)
accuracy(mb_t_box_prev, x = teste)

ICV_ts |> autoplot() + autolayer(mb_t_box_prev, PI = F) + autolayer(teste)

mb_t_box_prev2 = forecast(mb_t_box, h = 12, level = c(95), biasadj = T)
accuracy(mb_t_box_prev2, x = teste)
