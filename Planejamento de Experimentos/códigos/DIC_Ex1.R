# libs --------------------------------------------------------------------
library(ExpDes.pt)
library(tidyverse)

# dados -------------------------------------------------------------------
cb5034 = c(1123,1210,1143,1158,1172)/10; cb5034
cb6245 = c(1253,1197,1208,1205,1223)/10; cb6245
IAC6258 = c(1184,1205,1197,1183,1178)/10; IAC6258
IAC6529 = c(1279,1283,1295,1265,1273)/10; IAC6529
IAC6814 = c(1301,1224,1267,1273,1289)/10; IAC6814
IAC6538 = c(1152,1232,1178,1208,1164)/10; IAC6538

dados = tibble(cb5034 = cb5034,
               cb6245 = cb6245,
               IAC6258 = IAC6258,
               IAC6529 = IAC6529,
               IAC6814 = IAC6814,
               IAC6538 = IAC6538); dados


dados = dados |> mutate(Obs = 1:5) |> relocate(Obs, .before = cb5034); dados
dados2 = dados |> pivot_longer(!Obs, names_to = 'Tipo_Cana', values_to = 'Produção') |> arrange(Tipo_Cana); dados2
dados2$Tipo_Cana = as.factor(dados2$Tipo_Cana); dados2

# medidas descritivas -----------------------------------------------------------------

# usando o formato wide
medias_fatores = dados |> select(!Obs) |> colMeans(); medias_fatores
sd_fatores = dados |> select(!Obs) |> sapply(function(x) sd(x)); sd_fatores
cv_fatores = dados |> select(!Obs) |> sapply(function(x) sd(x)/mean(x)); round(cv_fatores*100, 2)

# usando o formato long
medias_fatores2 = tapply(dados2$Produção, dados2$Tipo_Cana, mean); medias_fatores2
sd_fatores2 = tapply(dados2$Produção, dados2$Tipo_Cana, sd); sd_fatores
cv_fatores2 = tapply(dados2$Produção, dados2$Tipo_Cana, function(x) sd(x)/mean(x)); cv_fatores


# modelagem ---------------------------------------------------------------
aov(dados2$Produção ~ dados2$Tipo_Cana)
anova(aov(dados2$Produção ~ dados2$Tipo_Cana))

## obtendo alguns resultados manualmente

N = length(dados2$Produção); N
s2 = var(dados2$Produção); s2
(N - 1)*s2 # SQTot
round(sqrt(6.256)/mean(dados2$Produção)*100, 2) # CV Estimado

115.250/6.256 # FCalc
qf(0.95, 5, 24, lower.tail = T) # FTab
qf(0.05, 5, 24, lower.tail = F) # FTab
pf(115.250/6.256, 5, 24, lower.tail = F) # P-Valor

# rejeitamos a hipótese nula de nulidade dos efeitos dos tratamentos

# teste de homogeneidade de variâncias
dic(trat = dados2$Tipo_Cana, resp = dados2$Produção, hvar = 'bartlett')

dados2 |> ggplot(aes(Tipo_Cana, Produção)) + 
  geom_boxplot() + 
  geom_point(colour = 'blue', size = 2)

