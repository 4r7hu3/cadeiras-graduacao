library(tidyverse)
library(olsrr, warn.conflicts = F)

# dados
x = c(1,1.5,2,3,4,4.5,5,5.5,6,6.5,7:15)
y = c(63,111,200,240,261,300,338,340,381,399,420,461,531,520,525,480,428,278,219)/10
Cx = x - mean(x); Cx

# plots
df = data.frame(Y = y, X = x, Cx = Cx); df
df %>% ggplot(aes(X, Y)) + geom_point() + labs(title="X vs Y")
df %>% ggplot(aes(Cx, Y)) + geom_point() + labs(title="X vs Y", subtitle = "X é a variável centralizada", x="X")

# modelos
mN = lm(y ~ x, df) # normal
mC = lm(y ~ Cx, df) # centralizado

# plot normal
df %>% ggplot(aes(X, Y)) + 
  geom_point() + 
  geom_smooth(method = lm) +
  labs(title="X vs Y")

# plot centralizado
df %>% ggplot(aes(Cx, Y)) + 
  geom_smooth(method=lm) +
  geom_point() + labs(title="X vs Y", subtitle = "X é a variável centralizada", x="X")

# diagnósticos
ols_plot_diagnostics(mN)
ols_plot_diagnostics(mC)
ols_plot_cooksd_chart(mN)
ols_plot_cooksd_chart(mC)

# anovas
anova(lm(y ~ model.matrix(mN)[,-1])) # normal
anova(lm(y ~ model.matrix(mC)[,-1])) # centralizado

# modelos polinomiais
mP = lm(y ~ x + I(x*x)); summary(mP)
plot(mP)
mPC = lm(y ~ Cx + I(Cx*Cx)); summary(mPC)
plot(mPC)

# função polinomial + plot
f = function(x){45.295 + 2.5463*x - 0.6345*x^2}
plot(Cx, y)
curve(f, add = T, col='red', lwd=2)

# com ggplot2
df %>% ggplot(aes(Cx, y)) + geom_point() + stat_function(fun=f, colour='red', linewidth=1.2)

# anovas dos modelos polinomiais
anova(lm(y ~ model.matrix(mP)[,-1]))
anova(lm(y ~ model.matrix(mPC)[,-1]))

# SQExtra
SQExtra = 3104.25 - 1043.4; SQExtra

# F0
F0 = SQExtra/19.54; F0
pf(F0,1,16,lower.tail = F)
