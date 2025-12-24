library(tidyverse)
library(ExpDes.pt)

df = read.delim2("~/Plan. Experimentos - Arthur/Cap3-14.txt") |> tibble(); df
df = df |> select(-RESI1); df

df |> ggplot(aes(as.factor(Temp), Density)) + 
  geom_point() + 
  geom_hline(yintercept = mean(df$Density), colour = 'red')


df |> ggplot(aes(as.factor(Temp), Density)) + 
  geom_boxplot() + 
  geom_point() 


dic_s0 = dic(df$Temp, df$Density); dic_s0

medias = tapply(df$Density, df$Temp, mean); medias

df |> ggplot(aes(as.factor(Temp), Density)) + 
  geom_point() + 
  geom_hline(yintercept = mean(df$Density), colour = 'red') + 
  geom_point(data = tibble(x = medias, y))
