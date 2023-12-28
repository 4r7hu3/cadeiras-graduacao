rm(list=ls())

getwd() 
setwd('/home/marilia/Documentos/R Avançado/Minicurso Analise de Dados com R') 


# Leitura das bases de dados ----------------------------------------------

library(readr) #Carregando o readr - para importação de dados.

inventory <- read_delim("inventory.csv",delim = ',') #importar um arquivo csv
View(inventory)
products <- read_delim("products.csv",delim = ',')
View(products)
sales <- read_delim("sales.csv",delim = ',')
View(sales)
stores <- read_delim("stores.csv", delim = ',')
View(stores)


# Estruturas das bases de dados -------------------------------------------

sum(is.na(inventory))  
sum(is.na(products))
sum(is.na(sales))
sum(is.na(stores))

head(inventory)
str(inventory)
head(products)
str(products)
head(stores)
str(stores)
head(sales)
str(sales)


# Estatísticas Descritivas, Histograma e Boxplot  -------------------------

# Cruzamento e visualização de dados --------------------------------------

library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)


# 1 Quais as três categorias de produtos geram os maiores lucros médios? 
#   Isso é o mesmo em todas as diferentes cidades?

cat_luc <-  products %>%
  inner_join(sales) %>%
  select(Product_Category, Product_Cost, Product_Price, Units) %>%
  group_by(Product_Category) %>%
  mutate(Lucro = Units*(parse_number(Product_Price)-parse_number(Product_Cost)))%>%
  summarise(lucro_medio = mean(Lucro)) %>%
  top_n(3,lucro_medio) %>% 
  arrange(desc(lucro_medio))
head(cat_luc)
View(cat_luc)

ggplot(cat_luc, aes(x = fct_reorder(Product_Category, lucro_medio), y=lucro_medio)) + 
  geom_col() +
  geom_label(aes(label = round(lucro_medio,2)),nudge_y = 1) +
  labs(x = "Categoria do produto",
       y="Lucro medio",title = "Categorias de produtos que geram maiores lucros medios") 


cat_luc_cit <-  products %>%
  inner_join(sales) %>% 
  inner_join(stores) %>%
  select(Product_Category, Product_Cost, Product_Price, Units, Store_City) %>%
  group_by(Store_City,Product_Category) %>%
  mutate(Lucro = Units*(parse_number(Product_Price)-parse_number(Product_Cost)))%>%
  summarise(lucro_medio = mean(Lucro)) %>% 
  top_n(3,lucro_medio)
head(cat_luc_cit)

ggplot(cat_luc_cit, aes(x=Store_City,y=lucro_medio, fill=Product_Category)) + 
  geom_col() + coord_flip() +
  labs(x = "Lojas",
       y="Lucro medio",
       fill="Categorias",
       title = "Categorias de produtos que geram os maiores lucros (medio) por cidade")



# 2 Você consegue identificar tendência nos dados mensais de vendas 
#   (quantidades) da categoria brinquedos (Toys)?
qtd_saz <- sales %>% 
  inner_join(products) %>% 
  select(Product_Category, Date, Units) %>% 
  filter(Product_Category =="Toys") %>% 
  mutate(dat = as.character(format(Date,"%Y-%m"))) %>% 
  group_by(dat) %>% 
  summarise(tot_prod_vend= sum(Units)) %>% 
  arrange(dat)

head(qtd_saz)


ggplot(qtd_saz, aes(x=dat, y=tot_prod_vend)) +
  geom_line(aes(group = 1)) +
  labs(x = 'Ano-Mes',
       y = 'Total de unidades vendidas',
       title = 'Total de vendas ao longo do tempo') +
  theme(axis.text.x = element_text(angle = 45))


# 3 (a) Em cada loja, para o produto Action Figure (categoria Toys), 
#   verifique quantos dias irá durar o estoque com base no número médio 
#   diario de produtos vendidos; (b) Em quais lojas o estoque é suficiente 
#   para 21 dias ou mais?

vend_est <- sales %>% 
  inner_join(inventory) %>% 
  inner_join(products) %>%
  inner_join(stores) %>% 
  select(Store_ID, Store_Name,Product_Name, Units, Stock_On_Hand,Date) %>%
  filter(Product_Name=="Action Figure",year(Date)==2018) %>% 
  group_by(Store_ID, Store_Name) %>% 
  summarise(med_vend = round(mean(Units),1),tot_est = mean(Stock_On_Hand), 
            dur = round(tot_est/med_vend))

head(vend_est)  

loj_dur <- vend_est %>% 
  filter(dur >= 21)

head(loj_dur)

# 4 Quais as cinco lojas com maiores valores (preço de custo) 
#   vinculados ao estoque?

est_din <- inventory %>% 
  inner_join(stores) %>%
  inner_join(products) %>%
  select(Stock_On_Hand,Store_Name, Product_Cost) %>%
  group_by(Store_Name) %>% 
  mutate(din=Stock_On_Hand*parse_number(Product_Cost)) %>%
  summarise(din_t=sum(din)) %>% 
  arrange(desc(din_t)) %>% 
  top_n(5, din_t)

head(est_din)

ggplot(est_din,aes(x=Store_Name,y=din_t)) + 
  geom_col(aes(fill = Store_Name)) + coord_flip() + 
  labs(x = "Lojas", y="Dinheiro") + 
  ggtitle("Valor total dos produtos em estoques") +
  guides(fill = FALSE) 



# 5 Para o ano de 2018, (a) Quais as 3 lojas com maiores (e com menores) 
#   lucros? (b) Em cada dia-mês, qual o lucro diário (total) em cada uma 
#   das 6 lojas? (c) Com base nos resultados do item (b),
#   para cada loja-mês, calcule o lucro médio diário e apresente os
#   resultados em um gráfico de linhas.

loj_luc <-  stores %>%
  inner_join(sales) %>%
  inner_join(products) %>% 
  select(Store_Name, Product_Cost, Product_Price, Units, Date) %>%
  filter(year(Date) == 2018) %>% 
  group_by(Store_Name) %>%
  mutate(Lucro = Units*(parse_number(Product_Price)-parse_number(Product_Cost)))%>%
  summarise(somalucro = sum(Lucro)) %>% 
  arrange(desc(somalucro))
View(loj_luc)
head(loj_luc,3) #três maiores lucros
tail(loj_luc,3) #três menores lucros


sum_md <- stores %>% 
  inner_join(sales) %>% 
  inner_join(products) %>% 
  select(Date, Units, Store_Name, Product_Price, Product_Cost) %>% 
  filter(year(Date) == 2018, Store_Name %in% c("Maven Toys Ciudad de Mexico 2",
                                               "Maven Toys Guadalajara 3",
                                               "Maven Toys Ciudad de Mexico 1",
                                               "Maven Toys La Paz 1",
                                               "Maven Toys Merida 1",
                                               "Maven Toys Toluca 2")) %>% 
  group_by(Store_Name, mes = month(Date), dia = day(Date)) %>% 
  mutate(Lucro = Units * (parse_number(Product_Price)-parse_number(Product_Cost))) %>% 
  summarise(Lucro_dia_mes = sum(Lucro)) 

media_lmc<- sum_md %>%
  group_by(Store_Name, mes) %>% 
  summarise(Lucro_medio_diario_mes = mean(Lucro_dia_mes))

head(media_lmc)

ggplot(media_lmc, aes(x = mes,y = Lucro_medio_diario_mes)) + 
  geom_line(aes(colour = Store_Name), size = 1.2) +
  scale_x_continuous(breaks = 1:9) +
  ggtitle("Lucros diario medio segundo mes e loja") +
  facet_wrap(~Store_Name) +
  theme(legend.position="bottom") +
  labs(y="Lucros diario medio")

# 6 Considerando todo o histórico de vendas, (a) Quais os lucros médio, 
#   mínimo e máximo, por tipo de localização? (b) Represente graficamente 
#   os lucros médios mensais das 4 localizações.

sum_tipo_loc <- sales %>%
  inner_join(products) %>%
  inner_join(stores) %>%
  select(Store_Location, Product_Cost, Product_Price, Units) %>%
  group_by(Store_Location) %>%
  mutate(lucro = Units*(parse_number(Product_Price)-parse_number(Product_Cost)))%>%
  summarise( minimo=min(lucro), luc_medio = mean(lucro),maximo=max(lucro))

head(sum_tipo_loc)

ggplot(sum_tipo_loc, aes(x = Store_Location, y = luc_medio))+
  geom_col(aes(fill = Store_Location)) + 
  geom_label(aes(label = round(luc_medio,2)),nudge_y = 0.1) +
  labs(x = "Locação", y="Lucro medio") +
  theme(legend.position="none")

# 7 Quais as três cidades com:  (a) Maiores lucros? (b) Maiores volumes 
#   de vendas (quantidades de produtos)?

cid_luc <- sales %>% 
  inner_join(stores) %>% 
  inner_join(products) %>% 
  select(Units, Store_City, Product_Price,Product_Cost) %>% 
  group_by(Store_City) %>% 
  mutate(Lucro = Units * (parse_number(Product_Price)-parse_number(Product_Cost))) %>% 
  summarise(soma_lucro = sum(Lucro)) %>% 
  top_n(3,soma_lucro)

head(cid_luc)

cid_unid <- sales %>% 
  inner_join(stores) %>% 
  select(Units, Store_City) %>% 
  group_by(Store_City) %>%  
  summarise(soma_unid = sum(Units)) %>% 
  top_n(3,soma_unid)
head(cid_unid)

ggplot(cid_luc, aes(x=fct_reorder(Store_City, -soma_lucro),y=soma_lucro)) +
  geom_col(fill = c('navyblue','darkred','darkgreen')) +
  geom_label(aes(label = soma_lucro), nudge_y  = 1) +
  labs(x = 'Cidades',
       y = 'Lucro total',
       title = 'Cidades com maiores lucros totais')

ggplot(cid_unid, aes(x=fct_reorder(Store_City, -soma_unid),y=soma_unid)) +
  geom_col(fill = c('navyblue','darkred','darkgreen')) +
  geom_label(aes(label = soma_unid), nudge_y  = 1) +
  labs(x = 'Cidades',
       y = 'Total de Unidades',
       title = 'Cidades com maiores quantidades vendidas')

# 8 Em qual(is) tipo(s) de locação(ões) estão as lojas mais rentáveis? E quais as 3
#   categorias de produtos mais rentáveis dessa(s) localização(ções). 
#   Apresente o total de unidades vendidas dessas categorias.

ren_loc <- products %>%
  inner_join(sales) %>%
  inner_join(stores) %>% 
  select(Store_Location, Product_Price, Product_Cost, Units) %>% 
  group_by(Store_Location) %>% 
  mutate(Lucro = Units * (parse_number(Product_Price) - parse_number(Product_Cost))) %>%
  summarise(Lucro_medio = mean(Lucro)) 
head(ren_loc)

ren_loc_aero <- products %>%
  inner_join(sales) %>%
  inner_join(stores) %>% 
  select(Store_Location, Product_Price, Product_Cost, Units, Product_Category) %>% 
  filter(Store_Location == 'Airport') %>% 
  group_by(Product_Category) %>% 
  mutate(Lucro = Units * (parse_number(Product_Price) - parse_number(Product_Cost))) %>%
  summarise(Lucro_medio = mean(Lucro), Total_Vendido = sum(Units)) %>% 
  top_n(3, Lucro_medio)
head(ren_loc_aero)
