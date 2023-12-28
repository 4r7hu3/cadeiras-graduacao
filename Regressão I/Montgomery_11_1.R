# dados
n = 10
p = 3

# somatórios
Sx1 = 223; Sx2 = 553; Sy = 1916; Sxx1 = 5200.9; Sxx2 = 31729; Sx1x2 = 12352
Sx1y = 43550.8; Sx2y = 104736.8; Syy = 371595.6

# médias
x1b = Sx1/n
x2b = Sx2/n
yb = Sy/n

# (X'X)^-1
m = matrix(c(n,Sx1,Sx2,Sx1,Sxx1,Sx1x2,Sx2,Sx1x2,Sxx2), nrow = 3); m
Ai = solve(m); Ai

# X'Y
s = matrix(c(Sy,Sx1y,Sx2y)); n

# betas
betas = Ai%*%s; betas

# função
f = function(x1,x2){betas[1] + betas[2]*x1 + betas[3]*x2}
f(18,43)

# SQRes e QMres
SQRes = Syy - t(betas)%*%s; SQRes
QMres = SQRes/(n-p); QMres

# SQTot
SQTot = Syy  - n*yb^2; SQTot

# SQReg
SQReg = SQTot-SQRes; SQReg

# QMreg e F0
QMreg = SQReg/(p-1); QMreg
F0 = QMreg/QMres; F0
pf(F0,p-1,n-p,lower.tail = F)

# erros padrões
Cjj = diag(Ai); Cjj
ep_b0 = sqrt(QMres*Cjj[1]); ep_b0
ep_b1 = sqrt(QMres*Cjj[2]); ep_b1
ep_b2 = sqrt(QMres*Cjj[3]); ep_b2

# estatística
t0_b1 = betas[2]/ep_b1; t0_b1
t0_b2 = betas[3]/ep_b2; t0_b2

# valores-p
2*pt(abs(t0_b1),n-p,lower.tail = F)
2*pt(abs(t0_b2),n-p,lower.tail = F)


