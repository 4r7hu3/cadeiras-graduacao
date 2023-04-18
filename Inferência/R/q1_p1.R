t = seq(0, 1, 0.1)
eqm1 = t*(1-t)
eqm2 = (0.5-t)**2
plot(t, eqm1, type = 'l', col = 'blue', ylab = 'eqm', xlab = 'teta')
lines(t, eqm2, type = 'l', col = 'red')
