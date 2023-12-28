# bibliotecas
library(tidyverse)
library(olsrr)
library(car)
library(lmtest)
getwd()
setwd("~/Downloads")

# dados
df = read.table('Ex11_04.txt', header = T); df
df =  df %>% select(-team) %>% rename('x1' = 'xl'); df
names(df) = str_to_lower(colnames(df)); df

# modelos
full.m = lm(y ~ ., df); summary(full.m) 
null.m = lm(y ~ 1, df); summary(null.m)

# bss
ols_step_best_subset(full.m, details = T)

# forward (aic e valor-p)
fw = ols_step_forward_aic(full.m, details = T)
ols_step_forward_p(full.m, details = T)
plot(fw)

# backward (aic e valor-p)
bw = ols_step_backward_aic(full.m, details = T)
ols_step_backward_p(full.m, details = T)
plot(bw)

# both direction (aic e valor-p)
bs = ols_step_both_aic(full.m, details = T)
ols_step_both_p(full.m, details = T)
plot(bs)

# modelos pretendentes
# a ordem das variáveis não é importante, neste caso
m1 = lm(y ~ x2+x4+x7+x8+x9, df); summary(m1)
m2 = lm(y ~ x2+x7+x8+x9, df); summary(m2)
m3 = lm(y ~ x2+x7+x8, df); summary(m3)

# análise de variância
anv.m1 = anova(lm(df$y ~ model.matrix(m1)[,-1])); anv.m1
anv.m2 = anova(lm(df$y ~ model.matrix(m2)[,-1])); anv.m2
anv.m3 = anova(lm(df$y ~ model.matrix(m3)[,-1])); anv.m3

# análise diagnóstica

## padrão do R
par(mfrow = c(2,2))
plot(m1)
plot(m2)
plot(m3)

## biblioteca
ols_plot_diagnostics(m1)
ols_plot_diagnostics(m2)
ols_plot_diagnostics(m3)

# verificação de pressupostos

## normalidade dos resíduos
ols_test_normality(m1)
ols_test_normality(m2)
ols_test_normality(m3)

## homocedasticidade
ols_test_breusch_pagan(m1) # bptest() ===> lmtest package
ols_test_breusch_pagan(m2)
ols_test_breusch_pagan(m3)

## multicolinearidade
ols_vif_tol(m1) # vif() ===> car package
ols_vif_tol(m2)
ols_vif_tol(m3)


# Funcao para criacao dos Envelopes Simulados (Juvêncio)
envel.norm <- function(modelo=fit.model,iden=0,nome=seq(along = model.matrix(modelo)[,1]),sim=100,conf=.90,res=T,quad=T) {
  
  #
  # Descrição e detalhes:
  # A saída será o gráfico de probabilidade normal com envelopes simulados para um ajuste da distribuição normal.
  #
  # A opção res=F faz o gráfico de probabilidade meio-normal com envelopes simulados utilizando a distância de Cook,
  # possibilitando a detecção de pontos simultaneamente aberrantes e/ou influentes.
  #
  # Atenção: a função não funcionará corretamente se o ajuste possuir offsets! Neste caso é preciso adaptá-la como foi
  # feito na função envel.pois
  #
  # Os dados devem estar disponíveis pelo comando attach( ).
  #
  # Argumentos obrigatórios:
  # modelo: deve-se informar o objeto onde está o ajuste do modelo normal linear, caso não seja informado, a função
  # 	  procurará o ajuste no objeto fit.model;
  #
  # Argumentos opcionais:
  # iden: caso deseje, informe o número de observações que irá querer destacar. O padrão é não destacar ninguém (iden=0).
  #	Qualquer valor que não seja um inteiro positivo (por ex., negativo ou decimal) fará com que a função pergunte
  #	o número de pontos após a execução;
  # nome: esse argumento só é utilizado caso seja destacado algum ponto no gráfico. Caso não seja informado nada, os pontos
  #	identificados serão os números da ordem em que estão no banco de dados (os índices). Caso se queira, pode-se
  #	informar um vetor de nomes ou de identificações alternativas. Obrigatoriamente esse vetor deve ter o mesmo
  #	comprimento do banco de dados;
  # sim: número de simulações para gerar a banda de confiança. Atkinson sugere um mínimo de 20 simulações.
  #      O padrão é de 100;
  # conf: nível de confiança do envelope. O padrão é de 90%;
  # res: permite-se a escolha se o gráfico será feito com os resíduos (res=T, True, padrão) ou com a distância de Cook
  #      (res=F, False);
  # quad: o padrão (quad=T, True) faz um gráfico quadrado, enquanto quad=F (False) faz um gráfico utilizando a área máxima
  #       disponível.
  #
  # Autor: Frederico Zanqueta Poleto <fred@poleto.com>, arquivo disponível em http://www.poleto.com
  #
  # Referências:
  # MCCULLAGH, P. e NELDER, J. A. (1989). Generalized Linear Models. 2ª ed. Chapman and Hall, London.
  # NETER, J., KUTNER, M. H., NACHTSHEIM, C. J. and WASSERMAN, W. (1996). Applied Linear Statistical Models. 4ª ed.
  #    Mc Graw Hill, Boston.
  # PAULA, G. A. (2003). Modelos de Regressão com apoio computacional. IME-USP, São Paulo. [Não publicado,
  #    disponível em http://www.ime.usp.br/~giapaula/Book.pdf]
  #
  # Exemplos:
  # envel.norm(ajuste,sim=10000,conf=.95)
  # envel.norm(ajuste,res=F)
  #
  
  if( class(modelo)[1]=="lm" || (class(modelo)[1]=="glm" && (modelo$family[[1]]=="Gaussian" | modelo$family[[1]]=="gaussian")) ) {
    
  } else {
    stop(paste("\nA classe do objeto deveria ser lm ou glm (com distribuicao gaussian) !!!"))
  }
  
  alfa<-(1-conf)/2
  X <- model.matrix(modelo)
  y<-predict(modelo)+resid(modelo)
  n <- nrow(X)
  p <- ncol(X)
  H <- X%*%solve(t(X)%*%X)%*%t(X)
  h <- diag(H)
  m <- fitted(modelo)
  
  #para evitar divisão por 0 ao studentizar os residuos, mas tentando manter o valor exagerado da alavanca
  h[round(h,15)==1]<-0.999999999999999
  
  si <- lm.influence(modelo)$sigma
  r <- resid(modelo)
  tsi <- r/(si*sqrt(1-h))
  sigma<-summary(modelo)$sigma
  ti <- r/(sigma*sqrt(1-h))
  di <- (1/p)*(h/(1-h))*(ti^2)
  
  e <- matrix(0,n,sim)
  e1 <- numeric(n)
  e2 <- numeric(n)
  
  for(i in 1:sim) {
    resp <- rnorm(n,m,sigma)
    fit <- lm(resp~X-1)
    ti<-resid(fit)/(summary(fit)$sigma*sqrt(1-h))
    if(res==F) {
      e[,i] <- (1/p)*(h/(1-h))*(ti^2)
    } else {
      e[,i] <- ti*sqrt( (n-p-1)/(n-p-(ti^2)) )
    }
    e[,i] <- sort(e[,i])
  }
  
  for(i in 1:n) {
    eo <- sort(e[i,])
    e1[i] <- quantile(eo,alfa)
    e2[i] <- quantile(eo,1-alfa)
  }
  
  med <- apply(e,1,median)
  
  if(quad==T) {
    par(pty="s")
  }
  if(res==F) {
    #Segundo McCullagh e Nelder (1989, pág.407) e Paula (2003, pág.57) deve-se usar qnorm((n+1:n+.5)/(2*n+1.125))
    #Segundo Neter et alli (1996, pág.597) deve-se usar qnorm((n+1:n-.125)/(2*n+0.5))
    qq<-qnorm((n+1:n+.5)/(2*n+1.125))
    plot(qq,sort(di),xlab="Quantil Meio-Normal",ylab="Distância de Cook", ylim=range(di,e1,e2), pch=16)
    nome<-nome[order(di)]
    r<-sort(di)
  } else {
    qq<-qnorm((1:n-.375)/(n+.25))
    plot(qq,sort(tsi),xlab="Quantil da Normal Padrão",ylab="Resíduo Padronizado", ylim=range(tsi,e1,e2), pch=16,main="Gráfico de envelope simulado com 95% de confiança")
    nome<-nome[order(tsi)]
    r<-sort(tsi)
  }
  lines(qq,e1,lty=1)
  lines(qq,e2,lty=1)
  lines(qq,med,lty=2)
  while ( (!is.numeric(iden)) || (round(iden,0) != iden) || (iden < 0) ) {
    cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
    out <- readline()
    iden<-as.numeric(out)
  }
  if(iden>0) {identify(qq,r,n=iden,labels=nome)}
  if(quad==T) {
    par(pty="m")
  }
  cat("\nBanda de ",conf*100,"% de confianca, obtida por ",sim," simulacoes.\n")
}

envel.norm(m1, conf = 0.95, quad = F, res = T)
envel.norm(m2, conf = 0.95, quad = F, res = T)
envel.norm(m3, conf = 0.95, quad = F, res = T)




