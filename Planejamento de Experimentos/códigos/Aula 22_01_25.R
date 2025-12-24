library(tidyverse)
library(MASS)
library(ExpDes.pt)
library(agricolae)

# Exemplo 1 ---------------------------------------------------------------

blocos = rep(1:4, 8); blocos
variedades = rep(LETTERS[1:8], each = 4); variedades
y = c(0.485,0.182,0.273,0.242,
      0.634,0.424,0.303,0.242,
      0.242,0.333,0.273,0.303,
      0.485,0.333,0.152,0.061,
      0.544,0.273,0.182,0.121,
      0.273,0.364,0.152,0.303,
      0.455,0.273,0.182,0.273,
      0.485,0.303,0.404,0.333); y


df = tibble(Variedade = variedades,
            Bloco = blocos,
            Resp  = y); df


head(df, 8)
tail(df)

hist(df$Resp)
shapiro.test(df$Resp)

bartlett.test(df$Resp, df$Variedade)
bartlett.test(df$Resp, df$Bloco)

df |> ggplot(aes(Variedade, Resp)) + 
  geom_boxplot() + 
  geom_point()


df |> ggplot(aes(as.factor(Bloco), Resp)) + 
  geom_boxplot() + 
  geom_point()


tapply(df$Resp, df$Variedade, mean)
tapply(df$Resp, df$Variedade, sd)
tapply(df$Resp, df$Bloco, mean)
tapply(df$Resp, df$Bloco, sd)

# respostas homogêneas
# para blocos e variedades

ExpDes.pt::dbc(df$Variedade, df$Bloco, df$Resp)

a = boxcox(lm(y ~ 1), plotit = T); names(a)
a$x[which.max(a$y)]

df = df |> mutate(Resp.t1 = asin(sqrt(Resp))); df

df |> ggplot(aes(Variedade, Resp.t1)) + 
  geom_boxplot() + 
  geom_point()

ExpDes.pt::dbc(df$Variedade, df$Bloco, df$Resp.t1)

# Exemplo 2 ---------------------------------------------------------------

df = read.delim("~/Plan. Experimentos - Arthur/Cap5-10.txt") |> tibble(); df

df |> group_by(Temperature) |> summarise(Media = mean(Light.Output),
                                         DP = sd(Light.Output),
                                         Min = min(Light.Output),
                                         Max = max(Light.Output))


df |> group_by(Glass.Type) |> summarise(Media = mean(Light.Output),
                                         DP = sd(Light.Output),
                                         Min = min(Light.Output),
                                         Max = max(Light.Output))


df |> ggplot(aes(Temperature, Light.Output, colour = as.factor(Glass.Type))) + 
  geom_point()


ExpDes.pt::fat2.dic(df$Glass.Type, 
                    df$Temperature, 
                    df$Light.Output, 
                    fac.names = c('Glass Type', 'Temperature'))


df |> ggplot(aes(as.factor(Temperature), Light.Output, group = Glass.Type)) + 
  geom_line()


bartlett.test(df$Light.Output ~ interaction(df$Glass.Type, df$Temperature))


ExpDes.pt::fat2.dic(df$Glass.Type, 
                    df$Temperature, 
                    df$Light.Output, 
                    fac.names = c('Glass Type', 'Temperature'), quali = c(T, F))


df |> ggplot(aes(Temperature, Light.Output, colour = as.factor(Glass.Type))) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F)

