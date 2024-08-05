library(tidyverse)
library(forecast)
library(latex2exp)
library(tseries)
library(patchwork)

# Lista I Aplicada

######## Questão 1 ########
plot.ts(rnorm(48)) # repetir várias vezes

# parece haver aleatoriedade

######## Questão 2 ########
plot.ts(rchisq(48, 2)) # repetir várias vezes

# parece haver aleatoriedade e claramente não normalidade

######## Questão 3 ########
proc = rnorm(50)
erro = rnorm(50, 0, 20)

##### a) 
a = function(t){100 + 10*t + erro}
plot.ts(a(proc), main = TeX("$X_t = 100 + 10t + \\epsilon_t$"), ylab = TeX("$X_t$"), xlab = 't')

##### b) 
b = function(t){100 - 10*t + 2*t**2 + erro}
plot.ts(b(proc), main = TeX("$X_t = 100 - 10t + 2t^2 + \\epsilon_t$"), ylab = TeX("$X_t$"), xlab = 't')

##### c) 
c = function(t){100 + 10*sin(2*pi*t/12) - 20*cos(2*pi*t/12) + erro}
plot.ts(c(proc), main = TeX("$X_t = 100 + 10\\sin{\\left(\\frac{2 \\pi t}{12}\\right)} - 20 \\cos{\\left(\\frac{2 \\pi t}{12}\\right)} + \\epsilon_t$"), ylab = TeX("$X_t$"), xlab = 't')

##### d)
d = function(t){100 + 10*t + 10*t*sin(2*pi*t/12) - 20*t*cos(2*pi*t/12) + erro}
plot.ts(d(proc), main = TeX("$X_t = 100 + 10t + 10t\\sin{\\left(\\frac{2 \\pi t}{12}\\right)} - 20tcos{\\left(\\frac{2 \\pi t}{12}\\right)} + \\epsilon_t$"), ylab = TeX("$X_t$"), xlab = 't')

##### e) 
e = function(t){100 + 10*sin(2*pi*t/12) - 20*cos(2*pi*t/12) - 5*sin(2*pi*t/6) + 30*cos(2*pi*t/6) + erro}
plot.ts(e(proc), main = TeX("$X_t = 100 + 10\\sin{\\left(\\frac{2 \\pi t}{12}\\right)} - 20\\cos{\\left(\\frac{2 \\pi t}{12}\\right)} - 5\\sin{\\left(\\frac{2 \\pi t}{6}\\right)} + 30\\cos{\\left(\\frac{2 \\pi t}{6}\\right)} + \\epsilon_t$"), ylab = TeX("$X_t$"), xlab = 't')

##### f)
f = numeric(50)
f[1] = 100 + erro[1]; f
for(i in 2:50){
  f[i] = 100 + 0.8*f[i-1] + erro[i]
}
plot.ts(f, main = TeX("$X_t = 100 + 0.8X_{t-1} + \\epsilon_t$"), ylab = TeX("$X_t$"), xlab = 't')

##### g) 
g = numeric(50); g
g[1] = 100 + erro[1]; g
for(i in 2:50){
  g[i] = 100 + erro[i] + 0.8*erro[i-1]
}
plot.ts(g, main = TeX("$X_t = 100 + \\epsilon_t + 0.8\\epsilon_{t-1}$"), ylab = TeX("$X_t$"), xlab = 't')

##### h) 
h = numeric(50); h
h[1] = 100 + erro[1]; h
for(i in 2:50){
  h[i] = 100 + erro[i] - 0.8*erro[i-1]
}
plot.ts(h, main = TeX("$X_t = 100 + \\epsilon_t - 0.8\\epsilon_{t-1}$"), ylab = TeX("$X_t$"), xlab = 't')


######## Questão 4 ########

##### a)
AirPassengers |> autoplot()

# há forte tendência e sazonalidade na série, que é mensal, e possui 144 observações (12 anos)
# a sazonalidade parece ser multiplicativa, pois a mesma cresce proporcionalmente ao nível da série

##### b)
AirPassengers |> decompose() |> autoplot()

# a tendência é praticamente linear
# os ruídos parecem ser aleatórios, apesar de haver alguns pontos sobressalentes (possíveis outliers)
# pela decomposição, a sazonalidade aparenta ser aditiva

##### c)
treino = AirPassengers |> head(length(AirPassengers) * 0.75); treino
teste = AirPassengers |> tail(-length(AirPassengers) * 0.75); teste


##### d)

# vamos considerar dois modelos: Holt-Winters aditivo e multiplicativo (pois temos claramente 3 componentes)
# como temos uma série longa, podemos considerar uma amortização na tendência
# a fim de melhor a acuracidade de previsões

# modelo aditivo
hw_a = hw(treino, seasonal = 'additive', damped = T)
hw_a$model # conferindo o modelo
hw_a$residuals |> ggtsdisplay() # há muitos significantes nas funções de autocorrelação
hw_a$residuals |> adf.test()
hw_a$residuals |> kpss.test()
hw_a$residuals |> Box.test(type = 'Ljung-Box')
hw_a$residuals |> shapiro.test()  
hw_a$model |> AIC()
hw_a_f = forecast(hw_a$model, h=36); hw_a_f # previsão de 3 anos

# aparentemente temos ruídos brancos, pois tanto
# o gráfico quanto a maioria dos testes 
# apontam que há estacionariedade

accuracy(hw_a_f, teste)

# tivemos valores baixos para a maioria dos erros
# porém o valor da estatística U ficou acima de 1
  
  
# modelo multiplicativo
hw_m = hw(treino, seasonal = 'multiplicative', damped = T)
hw_m$model # conferindo o modelo
hw_m$res |> ggtsdisplay() # menos significantes do que no aditivo
hw_m$res |> adf.test()
hw_m$res |> kpss.test()  
hw_m$res |> Box.test(type = 'Ljung-Box')
hw_m$res |> shapiro.test()
hw_m$model |> AIC()
hw_m_f = forecast(hw_m$model, h = 36); hw_m_f # previsão de 3 anos

# os resíduos parecem ser ruídos brancos
accuracy(hw_m_f, teste)


# desta forma, concluímos que o ajuste
# do modelo multiplicativo foi superior,
# pois apresentou menos lags significativos, 
# maior evidência de estacionariedade, 
# erros menores, além da estatística U < 1


# por fim, poderíamos usar uma função otimizada,
# ets(), para ver se encontramos um ajuste ainda melhor


ets_fit = ets(treino) # o método não gera previsões
ets_fit # conferindo o modelo
ets_fit$res |> ggtsdisplay() # ainda menos significantes que nos modelos anteriores
ets_fit$res |> adf.test()
ets_fit$res |> kpss.test()
ets_fit$res |> Box.test(type = 'Ljung-Box')
ets_fit$res |> shapiro.test()
ets_fit |> AIC()
ets_forecast = forecast(ets_fit, h = 36) # previsão de 3 anos

accuracy(ets_forecast, teste)

# o modelo gerado tem sazonalidade e ruídos
# na forma multiplicativa,
# e também uma tendência aditivia amortizada

# aparentemente, os resíduos são todos
# ruídos brancos, a partir dos testes
# e dos gráficos;
# obtemos também um modelo mais parcimonioso
# embora as previsões sejam apenas um pouco
# abaixo da performance do segundo modelo


##### gráficos de previsões
a = AirPassengers |> autoplot() + 
  autolayer(hw_a_f, PI = F) + 
  labs(title = 'Previsão: método aditivo', x = 'Tempo')


b = AirPassengers |> autoplot() + 
  autolayer(hw_m_f, PI = F) + 
  labs(title = 'Previsão: método multiplicativo', x = 'Tempo')


c = AirPassengers |> autoplot() + 
  autolayer(ets_forecast, PI = F) +
  labs(title = 'Previsão: ETS(M, Ad, M)', x = 'Tempo')


a / b / c
