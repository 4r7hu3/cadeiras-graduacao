# a) P(X>=120)
a = pnorm(120, 130, 8, lower.tail = F);a

# b) P(135<=X<=145)
b = pnorm(145, 130, 8) - pnorm(135, 130, 8);b
  
# c) P(X<120 ou X>=150)
c = pnorm(120, 130, 8) + pnorm(150, 130, 8, lower.tail = F);c