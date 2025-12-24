# Dados do experimento
dados <- matrix(c(59, 100, 87, 56,
                  58, 84, 71, 75,
                  74, 71, 66, 72,
                  91, 78, 100, 70), nrow = 4, byrow = TRUE)


### Layout dos Tratamentos ###

# B A C D
# D C B A 
# A B D C 
# C D A B

N = length(dados); N
a = sqrt(N); a
FC = (sum(dados)^2)/N

soma_tratamento = c(
  sum(dados[1, 2], dados[2, 4], dados[3, 1], dados[4, 3]),  # A
  sum(dados[1, 1], dados[2, 3], dados[3, 2], dados[4, 4]),  # B
  sum(dados[1, 3], dados[2, 2], dados[3, 4], dados[4, 1]),  # C
  sum(dados[1, 4], dados[2, 1], dados[3, 3], dados[4, 2])   # D
)


# Soma de Quadrados Totais
SQTot <- sum(dados^2) - FC; SQTot

# Soma de Quadrados das Linhas
SQLin = 1/a * sum(rowSums(dados)^2) - FC; SQLin

# Soma de Quadrados das Colunas
SQCol = 1/a * sum(colSums(dados)^2) - FC; SQCol

# Soma de Quadrados dos Tratamentos
SQTrat = 1/a * sum(soma_tratamento^2) - FC; SQTrat

# Soma de Quadrados dos Resíduos
SQRes = SQTot - SQTrat - SQLin - SQCol; SQRes

# Quadrado Médio dos Resíduos
QMRes = SQRes/((a-2)*(a-1)); QMRes

# Quadrado Médio dos Tratamentos
QMTrat = SQTrat/(a-1); QMTrat

# Quadrado Médio das Linhas
QMLin = SQLin/(a-1); QMLin

# Quadrado Médios das Colunas
QMCol = SQCol/(a-1); QMCol

# Estatística F
Fcalc = QMTrat/QMRes; Fcalc

# Valor crítico F tabelado, a 5% de significância
Ftab = qf(1-0.05,a-1,(a-2)*(a-1)); Ftab

# há evidências para rejeitar a hipótese nula
Fcalc > Ftab
pf(Fcalc, a-1, (a-2)*(a-1), lower.tail = F)

# Coeficiente de Variação
CV = sqrt(QMRes)/mean(dados); cat(round(CV*100, 2), "%")

### ExpDes.pt
library(ExpDes.pt)
?dql

tratamentos = matrix(c("B", "A", "C", "D",
                        "D", "C", "B", "A",
                        "A", "B", "D", "C",
                        "C", "D", "A", "B"), nrow = 4, byrow = TRUE)

# Criando o data frame
linhas = rep(1:4, each = 4)   
colunas = rep(1:4, times = 4) 
tratamento = as.vector(t(tratamentos))
resposta = as.vector(t(dados))

data_frame = data.frame(Linha = linhas,
                         Coluna = colunas,
                         Tratamento = tratamento,
                         Resposta = resposta); data_frame


dql(data_frame$Tratamento, data_frame$Linha, data_frame$Coluna, data_frame$Resposta)

# podemos afirmar que os tratamentos A e C tiveram impactos maiores e iguais na resposta
# em comparação com os tratamentos B e D

