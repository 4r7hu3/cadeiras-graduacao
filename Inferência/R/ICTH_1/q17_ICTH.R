x = c(152, 149, 132, 161, 121, 101, 122, 122, 143, 145, 151, 152, 150, 145, 141, 140, 142, 139, 139, 141)/10
n = length(x); n
xb = mean(x); xb
var_pop = 2;
var_amo = var(x); var_amo

# IC[mu; 95%]
gama = 0.95; alfa = 1-gama; gama; alfa; alfa/2
ztab = qnorm(1-alfa/2); round(ztab,2)
e = ztab*sqrt(var_pop)/sqrt(n); e
IC1_95 = xb+c(-1,1)*e; round(IC1_95,2)

# com erro = 1, qual o tamanho da amostra?
n_proc = ceiling((ztab*sqrt(var_pop)/1)^2); n_proc

# IC[sigma^2; 90%]
gama2 = 0.9; alfa2 = 1-gama2; gama2; alfa2; alfa2/2
num = (n-1)*var_amo; num
q2 = qchisq(1-alfa2/2,n-1); q2
q1 = qchisq(alfa2/2,n-1); q1
IC2_90 = c(num*1/q2,num*1/q1); round(IC2_90)

# supondo agora que var_pop é desconhecida
# construa um IC[mu; 90%]
ttab = qt(1-alfa2/2,n-1); round(ttab,2)
e2 = ttab*var_amo/sqrt(n)
IC3_90 = xb+c(-1,1)*e2; IC3_90

n2 = 30; xb2 = 13; var_amo_2 = 1

