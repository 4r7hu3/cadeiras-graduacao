#### Capítulo 1 - Morettin, Toloi, 2006, 2ª edição ####
library(tidyverse)
setwd('Documentos/UFC/Séries Temporais')

### Questão 2 ###
df = readxl::read_excel(paste0(getwd(), '/dados/','ICV.xls'))

# item a)
df |> ggplot(aes(`Mes/ano`, ICV)) +
  geom_line(colour = 'blue') + 
  labs(title = 'Índice do custo de vida em SP',
       subtitle = 'Observações mensais de janeiro de 1970 a junho de 1980',
       caption = expression(paste(bold('Não '), 'há estacionaridade na série')))


# item b)
diff1 = c(NA)
for(i in seq(1,dim(df)[1])){
  diff1 = append(diff1, df$ICV[i+1] - df$ICV[i])
}
diff1 = diff1[-length(diff1)]


df |> mutate(Dif1 = diff1) |> ggplot(aes(`Mes/ano`, `Dif1`)) +
  geom_line(colour = 'blue') +
  labs(title = 'Índice do custo de vida em SP - Primeira Diferença',
       subtitle = 'Observações mensais de janeiro de 1970 a junho de 1980',
       caption = expression(paste(bold('Não '), 'há estacionaridade na série')))
  

# item c)
diff2 = c(NA, NA)
for(i in seq(1,length(diff1))){
  diff2 = append(diff2, diff1[i+2] - diff1[i+1])
}
diff2 = diff2[-length(diff2)] # 2x


df |> mutate(Dif2 = diff2) |> ggplot(aes(`Mes/ano`, `Dif2`)) + 
  geom_line(colour = 'blue') +
  labs(title = 'Índice do custo de vida em SP - Segunda Diferença',
       subtitle = 'Observações mensais de janeiro de 1970 a junho de 1980',
       caption = expression(paste(bold('Há '), 'estacionaridade na série')))


### Questão 3 ###
df = 

# item a)
  
  
  
# item b)
  
  
### Questão 4 ###
  
# item a)


# item b)


# item c)


# item d)


