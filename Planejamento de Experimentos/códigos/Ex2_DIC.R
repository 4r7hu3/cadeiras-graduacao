library(tidyverse)
library(ExpDes.pt)

df = read.delim("~/Plan. Experimentos - Arthur/Cap3-19.txt") |> tibble(); df
# df$Orifice.Diameter = as.factor(df$Orifice.Diameter); df


df |> ggplot(aes(Orifice.Diameter, Radon.Released)) + 
  geom_point

df |> ggplot(aes(Orifice.Diameter, Radon.Released)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

df |> ggplot(aes(Orifice.Diameter, Radon.Released)) + 
  geom_boxplot() + 
  geom_point()


dic_op = dic(df$Orifice.Diameter, df$Radon.Released, quali = F); dic_op

ml_reg = lm(df$Radon.Released ~ df$Orifice.Diameter); ml_reg
anova(ml_reg)
aov(ml_reg)
summary(ml_reg)
summary(aov(ml_reg))




