nomes = c('Arthur', 'Tâmia', 'Adriana', 'Júlia', 'Kátia')
idades = c(19,29,43,37,39)

familia = data.frame(nome = nomes, idade = idades)
trabalho = c(F,T,T,T,F)
familia = data.frame(Nome = nomes, Idade = idades, Empregado = trabalho)
familia
names(familia)
familia["Nome"]
familia$Nome
familia[2]

f = c(F,F,F,T,T)
n = data.frame(Filhos = f)
familia = cbind(familia,n)
familia

#familia = familia[-4]
#familia

s = data.frame(Nome = 'Riarlei', Idade = 15, Empregado = F, Filhos = F)
familia = rbind(familia,s)
familia
