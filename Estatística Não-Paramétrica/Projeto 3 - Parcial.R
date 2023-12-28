library(purrr)
library(ggplot2)
set.seed(19)

# número de simulações
n_sim = 1000

# tamanhos amostrais
tam_amos = c(10, 20, 30, 50, 100)

# simulação partindo da distribuição normal
simul = function(n) {
  am1 = rnorm(n)
  am2 = am1 + rnorm(n, mean = 0.5)
  
  t_test = t.test(am1, am2, paired = TRUE)$p.value
  wilcox_test = wilcox.test(am1, am2, paired = TRUE)$p.value
  
  return(c(t_test, wilcox_test))
}

# replicar simulação n vezes
simul_n = function(n) {
  replicate(n_sim, simul(n))
}

# simulação com várias amostras
s = map(tam_amos, simul_n)

# poder dos testes para cada tamanho de amostra
poder.t = map_dbl(s, ~mean(.[1,] < 0.05))
poder.wilcoxon = map_dbl(s, ~mean(.[2,] < 0.05))

# dataframe dos resultados
df = data.frame(
  Amostra = tam_amos,
  Poder_T = poder.t,
  Poder_Wilcoxon = poder.wilcoxon
)

# gráfico comparativo
df |> ggplot(aes(tam_amos, colour="Teste")) +
  geom_line(aes(y = Poder_T, colour = 't')) + 
  geom_point(aes(y = Poder_T, colour = 't')) +
  geom_line(aes(y = Poder_Wilcoxon, colour = 'Wilcoxon')) + 
  geom_point(aes(y = Poder_Wilcoxon, colour = 'Wilcoxon')) +
  labs(x = "Tamanho amostral", y = "Poder do teste", title = "Teste t vs Teste de Wilcoxon") + 
  guides(color = guide_legend(title = "Testes")) + 
  scale_color_manual(values = c("t" = "blue", "Wilcoxon" = "red"))


#######################################################################################################

