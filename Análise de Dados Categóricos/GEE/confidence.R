# modeling clustered data with GEE

#packages
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}
if(!require(gee)){install.packages("gee");library(gee)}
if(!require(geepack)){install.packages("geepack");library(geepack)}

gss = read.csv("gss.confidence.csv") # "gss.confidence.csv"
gss$c.age = gss$age - mean(gss$age)
head(gss, 15) |> xtable(include = F)
glimpse(gss)
table(gss$question, gss$conf)

#Observando ototal devaloresnulosemcadavariavel
nulos = colSums(is.na(gss)) |>
  data.frame() |>
  rownames_to_column(var= 'Column') |>
  rename(NA_Freq = 'colSums.is.na.gss..');nulos

# all observations independent
fit.glm = glm(greatly~question, data=gss, family=binomial(link='logit'))
summary(fit.glm)

# gee with independent corr structure 
fit.ind = geeglm(greatly~question, data=gss, id=id, family=binomial, 
                 corstr="independence", scale.fix=T)
summary(fit.ind)
anova(fit.ind)

# gee with exchangeable corr structure
fit.exch = geeglm(greatly~question, data=gss, id=id, family=binomial, 
                  corstr="exchangeable", scale.fix=T)
summary(fit.exch)

# gee with unstructured corr structure
fit.un = geeglm(greatly~question, data=gss, id=id, family=binomial, corstr="unstructured", scale.fix=T)
summary(fit.un)

# gee with age covariate
fit.age = geeglm(greatly~question+age, data=gss, id=id, family=binomial, 
                 corstr="exchangeable", scale.fix=T)
summary(fit.age)


par(mfrow=c(2,2))
plot(fit.glm)
plot(fit.ind)





