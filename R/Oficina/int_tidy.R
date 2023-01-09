rm(list = ls())

library(tidyverse)
library(lubridate)
library(dplyr)
###############################################################################
#                      introdu��o ao tidyverse                                #
###############################################################################

################################ tidyr ########################################

############################### gather() ######################################

data("USArrests")

help("USArrests")

view(USArrests)

us_long <- USArrests %>% 
  gather(key = tipo_crime, value = 'taxa')

head(us_long)

USArrests$State <- rownames(USArrests)
head(USArrests)


uslong <- USArrests %>% 
  gather(key = 'tipo_crime', value = 'taxa', -State)


us_long_2 <- USArrests %>% 
  gather(key = 'tipo_crime', value = 'taxa', -c(State,UrbanPop))


############################### spread() ######################################

data("table2")

head(table2)
help("table1")


dados_espalhados <- table2 %>% 
  spread(key = type, value = count)


############################### separate() ####################################

data("table3")
head(table3)


dados_separados <- table3 %>% 
  separate(rate, into = c('cases', 'population'), sep = '/')


################################# unite() #####################################

data("table2")

head(table2)

dados_unidos <- table2 %>% 
  unite(type_year, type, year) %>% 
  spread(key = type_year, value = count)



################################### dplyr ###################################

data("midwest")

view(midwest)
help("midwest")
head(midwest)


################################### select() ################################

df1 <- midwest %>% 
  select(PID, county, state,poptotal, popdensity)

df2 <- midwest %>% 
  select(-state, -area)

################################# filter() ##################################


df3 <- midwest %>% 
  filter(popdensity >= 50000.0)


df4 <- midwest %>% 
  filter(popdensity >= 50000.0, popblack < 900000)


################################# arrange() ##################################


df5 <- midwest %>% 
  arrange(area)


################################## mutate() #################################

df6 <- midwest %>% 
  mutate(percentual_asiaticos = popasian / poptotal)


df7 <- midwest %>% 
  mutate(percasian = percasian / 100)

############################### summarise() #################################

df8 <- midwest %>% 
  filter(popdensity <= 1000) %>% 
  summarise(sum(popasian))


############################### group_by() #################################

df9 <- midwest %>% 
  group_by(state) %>% 
  summarise(mean(popblack))


################################ exerc�cio ##################################

install.packages("nycflights13")
library(nycflights13)

data("flights")
view(flights)
help("flights")

# fazer uma tabela com a média mensal dos atrasos e ordenadar pela
# média dos atrasos das partidas

levels(factor(flights$month))

atrasos_mensais <- flights %>% 
  select(month, arr_delay, dep_delay) %>% 
  group_by(month) %>%
  summarise(media_atraso_partida = mean(dep_delay, na.rm = TRUE),
            media_atraso_chegada = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(media_atraso_partida)
  

sum(is.na(flights$month))
sum(is.na(flights$arr_delay))
sum(is.na(flights$dep_delay))

head(atrasos_mensais, 12)

############################## ggplot2 ######################################

install.packages("gapminder") 
library(gapminder)

data("gapminder")
View(gapminder)

dados1 <- gapminder %>% 
  filter(year == max(year), continent == 'Americas')

dados2 <- gapminder %>% 
  filter(year %in% c(1957,2007)) %>%
  mutate(year = factor(year)) %>% 
  group_by(continent,year) %>%
  summarise(lifeExp = mean(lifeExp))

dados3 <- gapminder %>%
  mutate(year = factor(year)) %>%
  group_by(continent, year) %>% 
  summarise(lifeExp = mean(lifeExp))

dados4 <- gapminder %>%
  group_by(continent,year) %>% 
  summarise(lifeExp = mean(lifeExp))

data("iris")
help("iris")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = Species)) 

ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = Sepal.Length)) +
  scale_colour_gradient(low = 'red', high = 'yellow')


ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = Species)) +
  scale_x_continuous(name = 'comprimento da pétala',
                     breaks = seq(1,7,1)) +
  scale_y_continuous(name = 'largura da pétala', 
                     breaks = 0:3, limits = c(0,3))


data("diamonds")
view(diamonds)
help("diamonds")

ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
  geom_point() +
  facet_wrap(~cut, scales = 'free')

ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
  geom_point() +
  facet_grid(clarity~cut, scales = 'free')


ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_boxplot(aes(fill = Species)) +
  scale_x_discrete(name = 'espécies', labels  = c('','',''))

ggplot(gapminder, aes(x = factor(year), y = lifeExp)) +
  geom_boxplot(fill =  'dodgerblue')

ggplot(dados1, aes(x = country, y = lifeExp)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue') +
  labs(title = 'Expectativa de vida por pais',
       y = 'espectativa de vida',
       x = 'pais') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(dados2, aes(x = continent, y = lifeExp, fill = factor(year))) + 
  geom_bar (stat =  'identity', position = 'dodge' ,color = 'black')


ggplot(dados3, aes(x = continent, y = lifeExp, fill = year)) +
  geom_bar(stat = 'identity', position = 'dodge',color = 'grey')


ggplot(dados3, aes(x = continent, y = lifeExp, fill = year)) +
  geom_col(position = 'dodge',color = 'grey')


ggplot(dados4, aes(x = year, y = lifeExp, col = continent)) +
  geom_line() +
  geom_point() +
  labs(title = 'evolução da expectativa de vida por continente',
       x = 'ano',
       y = 'expectativa de vida',
       col = 'continente') +
  theme_dark()








