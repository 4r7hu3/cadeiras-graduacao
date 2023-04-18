# set.seed(508492)
n = 15
p = 0.4
x = 0:15
A = rbinom(x, n, p);A
table(A);A

# a) P(X>=14)
a = 1 - pbinom(14, n, p);a # mesmo que pbinom(14, n, p, lower.tail = F)

# b) P(8<X<=10)
b = pbinom(10, n, p) - pbinom(8, n, p);b

# c) P(X<2 ou X>=11)
c = pbinom(2, n, p) + pbinom(11, n, p, lower.tail = F);c

# d) P(X>=11 ou X>13)
d = pbinom(11, n, p, lower.tail = F) + pbinom(13, n , p, lower.tail = F) - pbinom(13, n, p, lower.tail = F);d

# e) P(X>3 e X<6)
e = pbinom(3, n, p, lower.tail = F) * pbinom(6, n , p);e

# f) P(X<=13|X>=11)
f = pbinom(13, n, p) * pbinom(11, n, p, lower.tail = F)/pbinom(11, n, p, lower.tail = F);f
