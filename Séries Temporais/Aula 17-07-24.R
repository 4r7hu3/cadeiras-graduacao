install.packages('tseries')
install.packages('forecast')
install.packages('tidyverse')

library(tseries)
library(forecast)
library(tidyverse)

getwd()
setwd('C:/Users/aluno/Documents/Arthur')

AirPassengers

AirPassengers %>% autoplot()

m0 = lm(force(AirPassengers) ~ force(time(AirPassengers)))
plot(AirPassengers)
abline(m0)

AirPassengers %>% autoplot() + geom_smooth(method = 'lm')

boxplot(AirPassengers ~ cycle(AirPassengers), col=cycle(AirPassengers))

decompose(AirPassengers) %>% autoplot()
decompose(AirPassengers, type = 'm') %>% autoplot() 

adf.test(AirPassengers)

forecast::ggtsdisplay(AirPassengers)

Box.test(AirPassengers, type='Ljung-Box')

ndiffs(AirPassengers)
forecast::ggtsdisplay(diff(AirPassengers)) # primeira dif
forecast::ggtsdisplay(log(AirPassengers)) # transformação
forecast::ggtsdisplay(diff(log(AirPassengers))) # diff 1 e transformação
forecast::ggtsdisplay(diff(diff(AirPassengers, lag = 12))) # dif saz e dif 1

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
AR = a = rnorm(1000)
for(t in 2:1000){
  AR[t] = 0.8*AR[t-1] + a[t]
}


# plots
AR %>% plot.ts()
forecast::ggtsdisplay(AR)
forecast::ggtsdisplay(diff(AR))

# testando dataset Morettin
library(readxl)
ICV = read_excel("C:/Users/aluno/Downloads/ICV.xls")
ICV_ts = ts(data = ICV$ICV, start = 1970, frequency = 12); ICV_ts

# plots
forecast::ggtsdisplay(ICV_ts)
forecast::ggtsdisplay(log(ICV_ts))
forecast::ggtsdisplay(diff(log(ICV_ts)))
forecast::ggtsdisplay(diff(diff(log(ICV_ts))))

# testes
adf.test(diff(log(ICV_ts)))
adf.test(diff(diff(log(ICV_ts))))
