# X ~ N(90, 100)
# a) P(X<=115)
a = pnorm(115, 90, 10);a

# b) P(X>=80)
b = pnorm(80, 90, 10, lower.tail = F);b

# c) P(X<=75)
c = pnorm(75, 90, 10);c

# d) P(85<=X<=110)
d = pnorm(110, 90, 10) - pnorm(85, 90, 10);d

# e) P(|X-90|<=10)
e = 2*pnorm(1) - 1;e

# f) P(90-a<=X<=90+a) = 0.95. Encontre o valor de a!
## sabemos que isso é:
## P(-a/10<=Z<=a/10) = 0.95
## P(X<=|Z|) = 0.95
## 2*phi(Z) - 1 = 0.95
f = 10*qnorm(1.95/2);f
# teste
round(pnorm(109.6, 90, 10) - pnorm(70.4, 90, 10), 2)
