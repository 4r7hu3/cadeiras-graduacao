y = c(154,136,91,125,133,125,93,80,132,107,142,115,114,120,141,
      108,90,54,89,93,77,54,50,94,76,96,74,79,71,90)


ym = matrix(y, ncol = 2); ym
yb = matrix(c(mean(ym[,1]), mean(ym[,2]))); yb
h0 = matrix(c(120,80)); h0
n = dim(ym)[1]; n
p = dim(ym)[2]; p


# VARIÂNCIA CONHECIDA
sigma = matrix(c(400,240,240,225), ncol = 2); sigma
Q = n %*% t(yb - h0) %*% solve(sigma) %*% (yb - h0); Q
q = qchisq(1-0.05, 2); q
pchisq(Q, 2, lower.tail = F)

ggplot() + 
  xlim(0,8) +
  geom_function(fun = dchisq, args = list(df = 2)) + 
  geom_vline(xintercept = Q, colour = 'darkgreen') + 
  geom_vline(xintercept = q, colour = 'darkblue', linetype = 'dashed') + 
  annotate('text', x = Q+1.5, y = 0.55, label = 'italic(Q) == 0.08971193', colour = 'darkgreen', parse = T, size = 3.5) +
  annotate('text', x = q+1.2, y = 0.55, label = 'italic(q) == 5.991465', colour = 'darkblue', parse = T, size = 3.5)


# VARIÂNCIA DESCONHECIDA
v_cov = cov(ym)
T2 = n %*% t(yb - h0) %*% solve(v_cov) %*% (yb - h0); T2
f = (n - p)/(p * (n-1)) * T2; f
f2 = qf(1-0.05, p, n-p); f2
pf(f, p, n-p, lower.tail = F)


ggplot() + 
  xlim(0,8) +
  geom_function(fun = df, args = list(df1 = p, df2 = n-p)) + 
  geom_vline(xintercept = f, colour = 'darkgreen') + 
  geom_vline(xintercept = f2, colour = 'darkblue', linetype = 'dashed') + 
  annotate('text', x = f+1.2, y = 0.95, label = 'italic(F) == 0.1774719', colour = 'darkgreen', parse = T, size = 3.5) +
  annotate('text', x = f2+1, y = 0.95, label = 'italic(f) == 3.805565', colour = 'darkblue', parse = T, size = 3.5)


