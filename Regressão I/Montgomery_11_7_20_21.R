y = c(193,230,172,91,113,125)
x1 = c(16,155,220,430,330,400)/10
x2 = c(851,816,1058,1201,1357,1115)

m = lm(y ~ x1+x2); summary(m)
f = function(x1,x2){m$coe[1] + m$coe[2]*x1 + m$coe[3]*x2}
f(25,1000)

X = model.matrix(m); X
mX = lm(y ~ X[,-1]); summary(mX)
anova(mX)

mxy = lm(y ~ x1+x1+x1*x2); summary(mxy)
anova(lm(y ~ model.matrix(mxy)[,-1]))


pf((12989.4-12161.6)/561.3,1,3,lower.tail = F)

anova(lm(y ~ x1))
anova(mX)
SQExtra = 12161.6 - 10240.4; SQExtra
F0 = SQExtra/650.1; F0
RY1_2 = SQExtra/1950.4; RY1_2




