# Capítulo 1 - Morettin, Toloi, 2006, 2ª edição
library(data.table)
library(tidyverse)
library(readxl)
library(patchwork)

# Questão 2 ####
df = read_excel('dados/ICV.xls')

# item a)
df |> ggplot(aes(`Mes/ano`, ICV)) +
  geom_line(colour = 'blue') + 
  labs(title = 'Índice do custo de vida em SP',
       subtitle = 'Observações mensais de janeiro de 1970 a junho de 1980',
       caption = expression(paste(bold('Não '), 'há estacionaridade na série')))


# item b)
p1 = df |> mutate(Dif1 = ICV - shift(ICV)) |> ggplot(aes(`Mes/ano`, Dif1)) +
  geom_line(colour = 'blue') +
  labs(title = 'Índice do custo de vida em SP - Primeira Diferença',
       subtitle = 'Observações mensais de janeiro de 1970 a junho de 1980',
       caption = expression(paste(bold('Não '), 'há estacionaridade na série')),
       y = 'Diferença 1 - ICV')


# item c)
p2 = df |> mutate(Dif1 = ICV - shift(ICV), Dif2 = Dif1 - shift(Dif1)) |> ggplot(aes(`Mes/ano`, Dif2)) + 
  geom_line(colour = 'blue') +
  labs(title = 'Índice do custo de vida em SP - Segunda Diferença',
       subtitle = 'Observações mensais de janeiro de 1970 a junho de 1980',
       caption = expression(paste(bold('Há '), 'estacionaridade na série')),
       y = 'Diferença 2 - ICV')

p1/p2

# Questão 3 ####
df = read_excel('dados/atmosfera.xlsx')

# item a)
df |> ggplot(aes(DATA, temperatura)) + geom_line(colour = 'blue') + 
  labs(title = 'Registros da temperatura ao meio dia em SP',
       subtitle = 'Observações diárias de janeiro a dezembro de 1997',
       x = 'Data',
       y = 'Temperatura',
       caption = expression(paste(bold('Não '), 'há estacionaridade')))


# item b)
p1 = df |> mutate(Dif1 = temperatura - shift(temperatura),
             Dif2 = Dif1 - shift(Dif1)) |> 
  ggplot(aes(DATA, Dif1)) +
  geom_line(colour = 'blue') +
  labs(title = 'Registros da temperatura ao meio dia em SP',
       subtitle = 'Observações diárias de janeiro a dezembro de 1977',
       x = 'Data',
       y = 'Diferença 1',
       caption = expression(paste(bold('Há '), 'estacionaridade')))


p2 = df |> mutate(Dif1 = temperatura - shift(temperatura),
                  Dif2 = Dif1 - shift(Dif1)) |> 
  ggplot(aes(DATA, Dif2)) +
  geom_line(colour = 'blue') +
  labs(title = 'Registros da temperatura ao meio dia em SP',
       subtitle = 'Observações diárias de janeiro a dezembro de 1977',
       x = 'Data',
       y = 'Diferença 2',
       caption = expression(paste(bold('Há '), 'estacionaridade')))

p1/p2
  
  
# Questão 4 ####
df = read_xls('dados/ENERGIA.XLS'); df

# completando os anos da série
for(i in 1:dim(df)[1]){
  if(is.na(df$Ano[i]) && df$Mes[i] != 1){
    df$Ano[i] = df$Ano[i-1]
  }
}; df

df = df |> mutate('Ano/Mes' = ym(paste(Ano, Mes, sep='-'))); df # criando nova coluna de data
df = df |> select(-Ano, -Mes) |> relocate(Energia, .after = 'Ano/Mes'); df # limpando

  
# item a)
df |> ggplot(aes(`Ano/Mes`, Energia)) + geom_line(colour = 'blue') + 
  labs(title = 'Consumo de energia elétrica em ES',
       subtitle = 'Valores mensais de janeiro de 1968 a setembro de 1979',
       x = 'Data',
       y = 'KwH',
       caption = expression(paste(bold('Há '), 'tendência')))


# item b)
df |> mutate(Diff1 = Energia-shift(Energia)) |> ggplot(aes(`Ano/Mes`, Diff1)) + 
  geom_line(colour = 'blue') + 
  labs(title = 'Consumo de energia elétrica em ES',
       subtitle = 'Valores mensais de janeiro de 1968 a setembro de 1979',
       x = 'Data',
       y = 'Primeira Diferença - KwH',
       caption = expression(paste(bold('Há '), 'estacionaridade')))


# item c)
df |> mutate(Log = log(Energia)) |> ggplot(aes(`Ano/Mes`, Log)) + 
  geom_line(colour = 'blue') + 
  labs(title = 'Consumo de energia elétrica em ES',
       subtitle = 'Valores mensais de janeiro de 1968 a setembro de 1979',
       x = 'Data',
       y = 'log(KwH)',
       caption = expression(paste(bold('Há '), 'tendência')))


# item d)
df |> mutate(Log = log(Energia), DifLog = Log-shift(Log)) |> ggplot(aes(`Ano/Mes`, DifLog)) + 
  geom_line(colour = 'blue') + 
  labs(title = 'Consumo de energia elétrica em ES',
       subtitle = 'Valores mensais de janeiro de 1968 a setembro de 1979',
       x = 'Data',
       y = 'Primeira Diferença log(KwH)',
       caption = expression(paste(bold('Há '), 'estacionaridade')))

# Questão 5 ####
df = read_table('dados/D-PETRO.txt'); df
df = df |> mutate(Data = seq(ymd(19950301), by = 'day', length.out = dim(df)[1])); df # criando coluna de datas


# item a)
df |> ggplot(aes(Data, Preco)) + geom_line(colour = 'blue') +
  labs(title = 'Preços diários das ações da Petrobrás PN',
       subtitle = 'Dados de 03/01/1995 a 04/07/1999',
       x = 'Data',
       y = 'Valor da Ação',
       caption = expression(paste(bold('Há '), 'tendência')))

# item b)
df |> mutate(Dif1 = Preco - shift(Preco)) |> ggplot(aes(Data, Dif1)) + 
  geom_line(colour = 'blue') + 
  labs(title = 'Preços diários das ações da Petrobrás PN',
       subtitle = 'Dados de 03/01/1995 a 04/07/1999',
       x = 'Data',
       y = 'Primeira Diferença - Valor da Ação',
       caption = expression(paste(bold('Há '), 'estacionaridade')))


# item c)
df |> mutate(Log = log(Preco)) |> ggplot(aes(Data, Log)) + 
  geom_line(colour = 'blue') +
  labs(title = 'Preços diários das ações da Petrobrás PN',
       subtitle = 'Dados de 03/01/1995 a 04/07/1999',
       x = 'Data',
       y = 'log(Valor da Ação)',
       caption = expression(paste(bold('Há '), 'tendência')))

# item d)
df |> mutate(Log = log(Preco), DifLog = Log - shift(Log)) |> ggplot(aes(Data, DifLog)) + 
  geom_line(colour = 'blue') +
  labs(title = 'Preços diários das ações da Petrobrás PN',
       subtitle = 'Dados de 03/01/1995 a 04/07/1999',
       x = 'Data',
       y = 'Primeira Diferença - log(Valor da Ação)',
       caption = expression(paste(bold('Há '), 'estacionaridade')))


# Questão 7 ####
df = read_table('dados/D-BANESPA.txt'); df

# item a)
library(e1071)
df |> summarise('Média' = mean(Preco),
                'Variância' = var(Preco),
                'Assimetria' = skewness(Preco),
                'Curtose' = kurtosis(Preco),
                '1Q' = quantile(Preco, 0.25),
                '2Q/Median' = median(Preco),
                '3Q' = quantile(Preco, 0.75),
                'Máx.' = max(Preco),
                'Mín.' = min(Preco))

# item b)
media_amostral = mean(df$Preco)
sd_amostral = sd(df$Preco)

df |> ggplot(aes(Preco)) + geom_histogram(bins = 20, aes(y = after_stat(density))) +
  stat_function(fun = dnorm, args = list(mean = media_amostral, sd = sd_amostral), colour = 'blue') +
  labs(title = expression(paste('Histograma vs ', 'N(',mu, '= mean(Preço) ; ', sigma, '= sd(Preço))')),
       x = 'Preço',
       y = 'Densidade')




  

