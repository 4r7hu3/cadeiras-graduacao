# set.seed(508492)
n = 15
p = 0.4
x = 0:15
A = rbinom(x, n, p);A
table(A);A

# a) p(X>=14)
a = 1 - pbinom(14, n, p);a # mesmo que pbinom(14, n, p, lower.tail = F)

# b) p(8<X<=10)
b = pbinom(10, n, p) - pbinom(8, n, p);b

# c) p(X<2 ou X>=11)
# os eventos são disjuntos (não há intersecção)
# logo, p(a U b) = p(a) + p(b) = p(X<2) + p(X>=11)
c = pbinom(2,n,p) - dbinom(2,n,p) + pbinom(11,n,p,lower.tail = F) + dbinom(11,n,p);c

# d) p(X>=11 ou X>13)
# p(a U b) = p(a) + p(b) - p(a INTER b) = p(X>=11) + p(X>13) - p(X>13) = p(X>=11)
d = pbinom(11, n, p, lower.tail = F) + dbinom(11,n,p);d

# e) p(X>3 e X<6)
# p(a INTER b) = p(3<X<6) = p(X<6) - p(X>3)
e = pbinom(6,n,p) - dbinom(6,n,p) - pbinom(3,n,p,lower.tail = 3);e

# f) p(X<=13|X>=11)
# p(a|b) = p(a INTER b)/p(b) = p(11<=X<=13)/p(X>=11) = [(p(X<=13) - p(X<11))]/p(X>=11) = [p(X<=13) - p(X<=10)]/p(X>=11)
f = (pbinom(13,n,p) - pbinom(10,n,p))/d;f
