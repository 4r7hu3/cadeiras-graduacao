# O símbolo # (jogo da velha) indica comentário

# Os símbolos   <-   ou    =  indicam atribuição

# O símbolo  ; (ponto e vírgula) separa instruções na mesma linha



#Operadores aritméticos e operadores lógicos(+ - * / : ^ %% == !=  > < <= >= & | )
#(adição,subtração,multiplicação,divisão,sequência,exponencial,módulo,igualdade,
#diferente de,maior que,menor que,menor igual,maior igual,e,ou)


#Exemplos de operações:


573+30

34-9

2*3

18/3

20:28

3^2

5%%2

4>5

3<4

20 <= 10^2

30>= 20^2

4 == 3
4 != 4

(30 > 10) & (4 < 3)
(30 > 10) | (4 < 3)

###################
#### Funções #####


### Funções iniciais ###

getwd() 	#Informa o diretório de trabalho atual
setwd("C:/Users/Eliane Freitas/OneDrive/Documentos")	# Estabelece o diretório de trabalho
install.packages(" ")     # Instala um pacote
library(nome_do_pacote)	; require(nome_do_pacote)   # Carregam um pacote
help(' '); ?função    # Mostra a documentação de um pacote ou função
help.search(' '); ?? # Faz uma pesquisa mais geral para o pacote/função
  example(' ')	    # Mostra exemplos de alguma função
# q() 	# Fecha a sessão
ls(); objects()  	# Exibe os objetos que foram armazenados
rm(x,y)  	# Remove o objeto 'x' e 'y'
rm(list=ls()); rm(list=objects()) # Remove todos os objetos que foram armazenados
str(objeto)    	# Mostra a estrutura de um objeto
class(objeto)	       # Verifica a classe de um objeto




### Funções Matemáticas ###

sqrt("colocar exemplo") # Raiz Quadrada
factorial() # Fatorial
exp();exp() # Exponencial
abs() # Absoluto
log() # Logaritmo Neperiano
round(pi, digits = 2) # Arredondamento com dois dígitos
ceiling(2.718281) # Arredondamento para cima
floor(2.718281) # Arredondamento para baixo




### Funções Estatísticas ###

length(c(2,3,7,9)) # Imprime o comprimento do vetor
mean(c(2,3,7,9)) # Calcula a média
median(c(2,3,7,9)) # Calcula a mediana
min(c(2,3,7,9)) # Imprime o valor mínimo
max(c(2,3,7,9)) # Imprime o valor máximo
var(c(2,3,7,9)) # Calcula a variância
sd(c(2,3,7,9)) # Calcula o desvio padrão
a=c(2,3,7,9)
summary(a)  # Mostra um resumo(estatístico) de 'a'





### Vetores ###


#Formas de criar um vetor-------

a <- 7 # a é um vetor do tipo númerico com uma única posição
a[1] ; a


b <- "Oficina de R" # b é um vetor de caractere
b 

bolsa <- 400:600
bolsa

Notas <- c(9,4,7,10)
Notas
summary(Notas)
Alunos <- c("Eliane","Maria","José","Francisco")
Alunos
summary(Alunos)


valores <- scan()  #comando scan

valores


c <- rep(5,10) ; c #função rep


#função seq -----
#seq(valor inicial, valor final (limite), incremento, comprimento da sequência)

seq(2,10) #seq(from,to)
seq(1,5,0.5) #seq(from,to,by) 
seq(2,10,length=5) # seq(from,to,length)
seq(from=3,by=10,length=5)






### Listas ###


x <- list(5, "kiwi", c(FALSE, TRUE, TRUE), 3 + 1i, 2L)

x[[3]] 
x[[5]]


x[c(3,1)]  # seleciona o 3º e o 1º componentes da lista x, nesta ordem

x[[4]]=3
x





### Matrizes ###

rm(list=objects()) 

matriz1 = matrix(1:18, nrow = 3, ncol = 6, byrow = FALSE) #função matrix,cria matriz
matriz1 #chamando o objeto matriz1
matriz2 = matrix(1:18, nrow = 3, ncol = 6) 
matriz2

matriz3 = matrix(1:18, nrow = 3, ncol = 6,byrow = TRUE)
matriz3


matriz1[,2] # todas as linhas, coluna 2
matriz1[1,] # linha 1, todas as colunas
matriz1[c(1,2),c(2,6)] # linhas 1 e 2, colunas 2 e 6
matriz1[1,2] # elemento da linha 1, coluna 2
matriz1[1,1:3] # linha 1, colunas de 1 a 3

matriz4 = matriz1[,c(1,3,5,6)] # criar a matriz matriz4 a partir das colunas 1,3,5 e 6 da matriz1
matriz4

matriz2[1,1:3] = c(10, 20 ,30) # substituir por 10, 20 e 30, os valores da linha 1, colunas de 1 a 3
matriz2
matriz2[,4] = 88 # substituir por 88, todos os valores da coluna 4 de matriz2
matriz2

matriz5 = matriz2[,-c(1,3)] # matriz5 formada pela matriz2 sem as colunas 1 e 3
matriz5

colnames(matriz5) = c('c1','c2','c3','c4') # adicionar rótulos às colunas da matriz5
matriz5

rownames(matriz5) = c('l1','l2','l3') # adicionar rótulos às linhas da matriz5
matriz5

matriz6 = matrix(1:4, ncol = 2, byrow = TRUE,
                 dimnames = list(c('col1','col2'), c('lin1','lin2'))) ; matriz6




A = matrix(1:4, ncol = 2, byrow = TRUE)

B = matrix(c(1, 1, 0, 1), ncol = 2, byrow = TRUE)

A ; B

A * B # multiplicação elemento a elemento 
A %*% B # multiplicação de matrizes
-A
t(A) # transposta da matriz A
det(A) # determinante da matriz A
diag(c(3, 9, 10:13)) # criar matriz diagonal

a = 5; b = 2; d = 3; diag(c(a + b, a * d, b - d, round(sqrt(a),1)))
A + 3 # Adição
B - 1 # Subtração
A * 3 # Multiplicação
B / 2 # Divisão






A;B
cbind(A,B) #Conecta vetores formando colunas de uma matriz com cada vetor (c = column)
rbind(A,B) #Conecta vetores formando linhas de uma matriz com cada vetor (r = row)






