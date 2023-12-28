# variáveis
y = c(16.5,14,6,10,3.5)
x1 = c(1,3.5,4,7.5,9)
x2 = c(2,3,4,5,6)

# modelo direto
m = lm(y ~ x1+x2); summary(m)

# modelo manual
X = model.matrix(m); X
Y = matrix(y); Y
Ai = solve(t(X)%*%X); Ai
H = X%*%Ai%*%t(X); H
mm = H%*%Y; mm
betas = Ai%*%t(X)%*%Y; betas

n = length(y); n
p = 3 # k variáveis + 1 
Jn = matrix(1, n, n); Jn
I = diag(n); I

# somas de quadrados
SQReg = t(Y)%*%(H - Jn/n)%*%Y; SQReg
SQRes = t(Y)%*%(I - H)%*%Y; SQRes
SQTot = sum((y - mean(y))^2); SQTot

# quadrados médios
QMreg = SQReg/(p-1); QMreg
QMres = SQRes/(n-p); QMres
QMTot = SQTot/(n-1); QMTot

# estatística F e valor-p
F0 = QMreg/QMres; F0
pf(F0, p-1, n-p, lower.tail = F)

# R²
SQReg/SQTot

# R² ajustado
1 - QMres/QMTot # ou: 1 - (n-1)/(n-p)*SQRes/SQTot

# erros padrões
Cjj = diag(Ai); Cjj
er_b0 = sqrt(QMres*Cjj[1]); er_b0
er_b1 = sqrt(QMres*Cjj[2]); er_b1
er_b2 = sqrt(QMres*Cjj[3]); er_b2

# estatística t
t0_0 = betas[1]/er_b0; t0_0
t0_1 = betas[2]/er_b1; t0_1
t0_2 = betas[3]/er_b2; t0_2

# valores de p das estatísticas
vp1 = 2*pt(abs(t0_0), n-p, lower.tail = F); vp1
vp2 = 2*pt(abs(t0_1), n-p, lower.tail = F); vp2
vp3 = 2*pt(abs(t0_2), n-p, lower.tail = F); vp3

# erro padrao residual
sqrt(QMres)

# tabela anova real
mX = lm(Y ~ X[,-1]); mX 
anova(mX)

# intervalos de confiança p/ betaJ
IC_B0 = betas[1]+c(-1,1)*qt(1-0.05/2, n-p)*er_b0; IC_B0
IC_B1 = betas[2]+c(-1,1)*qt(1-0.05/2, n-p)*er_b1; IC_B1
IC_B2 = betas[3]+c(-1,1)*qt(1-0.05/2, n-p)*er_b2; IC_B2
confint(m)
confint(mX)
