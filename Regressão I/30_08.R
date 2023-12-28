# dados
peso = c( 12,13,14, 16,16,18, 19,19, 22,22, 26)/10
des =  c(16, 12, 12, 14, 10, 14, 12, 9.9, 10, 7, 7.5)

# primeiro olhar
plot(peso,des, pch = 19)

# estatísticas iniciais
n = length(peso)
xb = mean(peso); yb = mean(des)
Sxy = sum(des*peso); Sxx = sum((peso - xb)*peso); Syy = sum((des - yb)*des); SXY = sum((peso - xb)*des)
Sx = sum(peso); Sy = sum(des)
beta1 = SXY/Sxx
beta0 = yb - beta1*xb

# MRLS
reg_mod = lm(des ~ peso); summary(reg_mod)

# função e valores ajustados + plot
ma = function(peso) {as.numeric(reg_mod$coe[1] + reg_mod$coe[2]*peso)}
ma(peso)
reg_mod$residuals
abline(reg_mod, col = 'blue',lwd = 3)
points(peso, ma(peso), col='orange',pch=19,lwd=8)

# guardando os previstos e verificando os resíduos
previstos = ma(peso)
residuos = des - previstos
sumario = cbind(peso,des,previstos,residuos); sumario
summary(reg_mod$residuals)
summary(residuos)

# erro padrão dos estimadores beta0 e beta1
QMres = sum((des - previstos)^2)/(n-2)
QMreg = sum((previstos - yb)^2)
Sdb0 = sqrt(QMres*((1/n) + xb^2/(Sxx))); Sdb0
Sdb1 = sqrt(QMres/Sxx); Sdb1

# estatística de teste para beta0 e beta1
T0_0 = beta0/Sdb0; T0_0
T0_1 = beta1/Sdb1; T0_1

# níveis descritivos para beta0 e beta1
ND0 = 2*pt(abs(T0_0),n-2,lower.tail = F); ND0 # 2*min(pt(T0_0,n-2), pt(T0_0,n-2, lower.tail=F))
ND1 = 2*pt(abs(T0_1),n-2,lower.tail = F); ND1 # 2*min(pt(T0_1,n-2), pt(T0_1,n-2, lower.tail=F))

# E.P. residual, R² e R² ajustado
Sdres = sqrt(QMres); Sdres
R2 = cor(des,peso)^2; R2 # sum((previstos - yb)^2)/sum((des - yb)^2)
R2_adj = 1 - (n-1)/(n-2)*(1-R2); R2_adj

# estatística F e valor-p
F0 = QMreg/QMres; F0
ND_F = pf(F0,1,n-2,lower.tail = F); ND_F


#### Análise de Resíduos #####

# gráficos
plot(peso,reg_mod$residuals, ylab = 'resíduo ordinário')
abline(h=0, col='grey', lwd=3)
plot(reg_mod$fitted.values,reg_mod$residuals, xlab = 'des. previsto', ylab = 'resíduo ordinário')
abline(h=0, col='grey', lwd=3)
qqnorm(reg_mod$residuals)
qqline(reg_mod$residuals)

# resíduos padronizados e estudentizados
residuos_pdr = residuos/Sdres
residuos_std = residuos/(Sdres*sqrt(1-(1/n + (peso - xb)^2/Sxx)))

plot(peso,residuos_pdr, ylab='resíduo padronizado')
abline(h=0, col='grey', lwd=3)
plot(peso,residuos_std, ylab='resíduo studentizado')
abline(h=0, col='grey', lwd=3)

# teste de normalidade
shapiro.test(reg_mod$residuals)

# plotando tudo direto
plot(reg_mod)

# Concluímos que o modelo tem um bom ajuste, a partir dos gráficos acima,
# pois nossas suposições foram satisfeitas





