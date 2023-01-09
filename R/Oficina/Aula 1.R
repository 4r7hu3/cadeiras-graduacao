#passando os dados para uma variável e imprimindo em seguida
dados = Empresa
str(dados)

salario = as.numeric(dados$salario) #transforma os valores strings para valores numéricos

#tendência central 
mean(salario)
#sum(salario)
#length(salario)
sort(salario)
median(salario)
table(salario) #para encontrar o(s) valor(es) modal(is)

#dispersão
max(salario) - min(salario)
var(salario)
sqrt(var(salario))
#sd(salario) -> desvio padrão de forma direta

#gráficos
hist(salario, main = "Salário", ylab = "Frequência")
boxplot(salario, main = "Boxplot")
instrucao = dados$instrucao
table(dados$instrucao)
barplot(instrucao)
barplot(dados$civil)
barplot(table(dados$civil))

dados$filhos
filhos = dados$filhos[!dados$filhos == "NA"] #retirada dos valores 'NA' do vetor
filhos = as.numeric(filhos)
mean(filhos)
var(filhos)
sd(filhos)

plot(dados$salario, dados$anos)

##################################################

p.lucro = c(45, 60, 54, 62, 55, 70, 38, 48, 64, 55, 56, 55, 54, 59, 48, 65, 55, 60)
mean(p.lucro)
sd(p.lucro)
100*sd(p.lucro)/mean(p.lucro) #coeficiente de dispersão
sort(p.lucro)
median(p.lucro)
summary(p.lucro)
boxplot(p.lucro, main = "Boxplot do lucro")

#################################################

#criando funções e passando parâmetros
fx = function(x) {
  fx = ((x^2) + sqrt(3))/7
   return(fx)
}

fx(3)
x1 = c(1, 2, 3, 4, 5)
fx(x1) #0u fx(1:5)
plot(fx, 0, 5)

gx = function(x) {
  gx = log2(x^5 + 5)
  return(gx)
}

x2 = c(1, 3, 4)
gx(x2)
plot(gx, 0, 5)


hx = function(x) {
  hx = log(x^5 + 5)
  return(hx)
}

plot(hx, 0, 1)
abline(v = 0.4, col = "red", lwd = 1)


mx = function(x) {
  mx = exp(abs(x) - 2)
  return(mx)
}

plot(mx, -10, 10)
mx(3)
abline(h = mx(3), col = "blue", lwd = 1)


qx = function(a, b, d, x) {
  qx = a*(x^2) + b*x + d
  return(qx)
}

a = 1
b = 2
d = 1
x = seq(-3, 3, 0.1); x
y = qx(a, b, d, x)

plot(x, y, type = "l")
