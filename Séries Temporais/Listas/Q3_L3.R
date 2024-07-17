library(forecast)
library(tidyverse)

# dados
dados = data.frame(Zt = c(3,2,4,6,3,5,4,7,6,3,5,8,4,9,10,8)); dados
dados_ts = ts(dados, start = 1962, frequency = 4); dados_ts

# gráficos da série
autoplot(dados_ts) + labs(title = 'Série Trimestral: 1962 a 1965', caption = 'Série com tendência e sazonalidade estocástica', x = 't', y = 'Zt')
autoplot(decompose(dados_ts))

# Por termos tendência e sazonalidade (estocástica) presentes, 
# parece razoável supor um modelo autoregressivo ou de médias móveis (ou ambos) após diferenciação.
# Ou, também pode-se aplicar um modelo Holt-Winters, sem precisar aplicar diferenciação.

# estimação

## tendência
t_hat = ma(dados_ts, 4); t_hat # mesmo que decompose(dados_ts)$trend
Zt_TA = dados_ts - t_hat; Zt_TA # série com tendência ajustada

# gráfico 1
autoplot(dados_ts, series = 'Zt') + 
  autolayer(t_hat, series = 'T*') + 
  scale_colour_manual(values = c('Zt' = 'grey50', 'T*' = 'red')) +
  guides(colour = guide_legend(title = 'Séries', reverse = T)) +
  labs(title = 'Zt vs T*', x = 't', y = 'Zt')

# gráfico 2
autoplot(Zt_TA) + labs(title = 'Zt com tendência ajustada', x = 't', y = 'Zt_TA', caption = 'Ficamos com Zt* = Zt - T*')

## sazonalidade
Y_bar = mean(Zt_TA, na.rm = T); Y_bar
y1_bar = (-1.5+0.875-3.125)/3
y2_bar = (0.375-2.375+1.25)/3
y3_bar = (0.25-1.125-0.25)/3
y4_bar = c(1.875+1.75+2.25)/3
S1_hat = y1_bar - Y_bar; S1_hat
S2_hat = y2_bar - Y_bar; S2_hat
S3_hat = y3_bar - Y_bar; S3_hat
S4_hat = y4_bar - Y_bar; S4_hat
Zt_SA = dados_ts - decompose(dados_ts)$season; Zt_SA

# gráfico 1
autoplot(dados_ts, series = 'Zt') +
  autolayer(Zt_SA, series = 'S*') +
  scale_color_manual(values = c('Zt' = 'grey50', 'S*' = 'red')) +
  guides(colour = guide_legend(title = 'Séries', reverse = T)) +
  labs(title = 'Zt vs S*', x = 't', y = 'Zt')

# gráfico 2
s_hat = decompose(dados_ts)$season
at = dados_ts - t_hat - s_hat; at
checkresiduals(at)

# Com os gráficos, vemos que há indícios de não aleatoriedade,
# uma vez que a variância não é constante, e a fac não fica 
# próxima a 0 em vários lags.

# Pelo teste Ljung-Box, H0: aleatoriedade vs H1: não aleatoriedade.
# Como o nível descritivo < 5%, rejeitamos a hipótese de aleatoriedade dos resíduos.

# estimação da tendência por mínimos quadrados
N = length(dados_ts); N
St = sum(1:16); St
SZt = sum(dados_ts); SZt
tbar = St/N; tbar
ztbar = SZt/N; ztbar
St2 = sum((1:16)**2); St2
StZt = sum((1:16)*dados_ts); StZt

b1 = (StZt - N*tbar*ztbar)/(St2 - N*tbar**2); b1
b0 = ztbar - b1*tbar; b0

t = function(t){
  return(b0 + b1*t)
}

t(dados_ts)






