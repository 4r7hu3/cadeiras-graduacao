# O s?mbolo # (jogo da velha) indica coment?rio

# Os s?mbolos   <-   ou    =  indicam atribui??o

# O s?mbolo  ; (ponto e v?rgula) separa instru??es na mesma linha



#Operadores aritm?ticos e operadores l?gicos(+ - * / : ^ %% == !=  > < <= >= & | )
#(adi??o,subtra??o,multiplica??o,divis?o,sequ?ncia,exponencial,m?dulo,igualdade,
#diferente de,maior que,menor que,menor igual,maior igual,e,ou)


#Exemplos de opera??es:


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
#### Fun??es #####


### Fun??es iniciais ###

getwd() 	#Informa o diret?rio de trabalho atual
setwd("C:/Users/Eliane Freitas/OneDrive/Documentos")	# Estabelece o diret?rio de trabalho
install.packages(" ")     # Instala um pacote
library(nome_do_pacote)	; require(nome_do_pacote)   # Carregam um pacote
help(' '); ?fun??o    # Mostra a documenta??o de um pacote ou fun??o
help.search(' '); ?? # Faz uma pesquisa mais geral para o pacote/fun??o
  example(' ')	    # Mostra exemplos de alguma fun??o
# q() 	# Fecha a sess?o
ls(); objects()  	# Exibe os objetos que foram armazenados
rm(x,y)  	# Remove o objeto 'x' e 'y'
rm(list=ls()); rm(list=objects()) # Remove todos os objetos que foram armazenados
str(objeto)    	# Mostra a estrutura de um objeto
class(objeto)	       # Verifica a classe de um objeto




### Fun??es Matem?ticas ###

sqrt("colocar exemplo") # Raiz Quadrada
factorial() # Fatorial
exp();exp() # Exponencial
abs() # Absoluto
log() # Logaritmo Neperiano
round(pi, digits = 2) # Arredondamento com dois d?gitos
ceiling(2.718281) # Arredondamento para cima
floor(2.718281) # Arredondamento para baixo




### Fun??es Estat?sticas ###

length(c(2,3,7,9)) # Imprime o comprimento do vetor
mean(c(2,3,7,9)) # Calcula a m?dia
median(c(2,3,7,9)) # Calcula a mediana
min(c(2,3,7,9)) # Imprime o valor m?nimo
max(c(2,3,7,9)) # Imprime o valor m?ximo
var(c(2,3,7,9)) # Calcula a vari?ncia
sd(c(2,3,7,9)) # Calcula o desvio padr?o
a=c(2,3,7,9)
summary(a)  # Mostra um resumo(estat?stico) de 'a'





### Vetores ###


#Formas de criar um vetor-------

a <- 7 # a ? um vetor do tipo n?merico com uma ?nica posi??o
a[1] ; a


b <- "Oficina de R" # b ? um vetor de caractere
b 

bolsa <- 400:600
bolsa

Notas <- c(9,4,7,10)
Notas
summary(Notas)
Alunos <- c("Eliane","Maria","Jos?","Francisco")
Alunos
summary(Alunos)


valores <- scan()  #comando scan

valores


c <- rep(5,10) ; c #fun??o rep


#fun??o seq -----
#seq(valor inicial, valor final (limite), incremento, comprimento da sequ?ncia)

seq(2,10) #seq(from,to)
seq(1,5,0.5) #seq(from,to,by) 
seq(2,10,length=5) # seq(from,to,length)
seq(from=3,by=10,length=5)






### Listas ###


x <- list(5, "kiwi", c(FALSE, TRUE, TRUE), 3 + 1i, 2L)

x[[3]] 
x[[5]]


x[c(3,1)]  # seleciona o 3? e o 1? componentes da lista x, nesta ordem

x[[4]]=3
x





### Matrizes ###

rm(list=objects()) 

matriz1 = matrix(1:18, nrow = 3, ncol = 6, byrow = FALSE) #fun??o matrix,cria matriz
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

colnames(matriz5) = c('c1','c2','c3','c4') # adicionar r?tulos ?s colunas da matriz5
matriz5

rownames(matriz5) = c('l1','l2','l3') # adicionar r?tulos ?s linhas da matriz5
matriz5

matriz6 = matrix(1:4, ncol = 2, byrow = TRUE,
                 dimnames = list(c('col1','col2'), c('lin1','lin2'))) ; matriz6




A = matrix(1:4, ncol = 2, byrow = TRUE)

B = matrix(c(1, 1, 0, 1), ncol = 2, byrow = TRUE)

A ; B

A * B # multiplica??o elemento a elemento 
A %*% B # multiplica??o de matrizes
-A
t(A) # transposta da matriz A
det(A) # determinante da matriz A
diag(c(3, 9, 10:13)) # criar matriz diagonal

a = 5; b = 2; d = 3; diag(c(a + b, a * d, b - d, round(sqrt(a),1)))
A + 3 # Adi??o
B - 1 # Subtra??o
A * 3 # Multiplica??o
B / 2 # Divis?o






A;B
cbind(A,B) #Conecta vetores formando colunas de uma matriz com cada vetor (c = column)
rbind(A,B) #Conecta vetores formando linhas de uma matriz com cada vetor (r = row)






