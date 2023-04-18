# a) X ~ Pois(5)
x = 0:100
plot(x, dpois(x, 5), type = 'l')

# b) X ~ N(90, 100)
y = 50:130
plot(function(y) dnorm(y, 90, 10), 50, 130, type = 'l')

# c) Y ~ N(90, 80), Z ~ N(85, 100)
plot(function(y) dnorm(y, 90, 10), 50, 130, type = 'l', ylim=c(0, 0.05))
plot(function(y) dnorm(y, 90, sqrt(80)), 50, 130, type = 'l', col = 2, add=T)
plot(function(y) dnorm(y, 85, 10), 50, 130, type = 'l', col = 3, add=T)
legend("topleft", c("N(90, 100)", "N(90, 80)", "N(85, 100)"), fill=1:3)

# c) X^2 com n graus de liberdade, n em {1, 2, 5}
curve(dchisq(x, 1), from = 1 , to = 20, ylim=c(0, 0.3))
curve(dchisq(x, 2), from = 1, to = 20, add=T, col = 2)
curve(dchisq(x, 5), from = 1, to = 20, add=T, col = 3)
legend("topright", c("Chi2(1)", "Chi2(2)", "Chi2(5)"), fill = 1:3)
