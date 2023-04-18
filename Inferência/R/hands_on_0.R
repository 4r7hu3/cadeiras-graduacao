data("mtcars")
mtcars
dim(mtcars)
head(mtcars) # é o mesmo que mtcars[1:5,]
names(mtcars)

# cut da base
mtc = mtcars[, c(1, 2, 4, 6, 9, 10)]
head(mtc)
names(mtc)

# anexando o objeto
attach(mtc)

# partindo para descrições
## número de cilíndros
tcyl = table(cyl)
barplot(tcyl)
pie(tcyl)

tcyl = 100*table(cyl)/length(cyl) # é o mesmo que prop.table(tcyl)
tcyl
barplot(tcyl)
pie(tcyl)

## milhas por galão
table(cut(mpg, breaks = seq(10, 35, 5)))
hist(mpg)
boxplot(mpg)
stem(mpg)
summary(mpg)


# bivariadas
table(am, cyl)
prop.table(table(am, cyl), margin = 1)
prop.table(table(am, cyl), margin = 2)
plot(table(am, cyl))
barplot(table(am, cyl), legend.text = T)
barplot(table(am, cyl), beside = T, legend.text = T)

# câmbio vs rendimento
tapply(mpg, am, summary)
plot(am, mpg)
m0 = mean(mpg[am == 0]);m1
m1 = mean(mpg[am != 0]);m0
points(c(0, 1), c(m0, m1), cex = 2, col = 2, pch = 20)
par(mfrow = c(1, 2))
by(hp, am, hist)
par(mfrow = c(1, 1))

# teste t
t.test(mpg[am == 0], mpg[am !=0])
tapply(mpg, cyl, mean)
plot(cyl, mpg)
anova(aov(mpg ~ cyl))


# peso x rendimento
plot(wt, mpg)
cor(wt, mpg)


points(wt[cyl==4], mpg[cyl==4], col=2, pch=19)
points(wt[cyl==6], mpg[cyl==6], col=3, pch=19)
points(wt[cyl==8], mpg[cyl==8], col=4, pch=19)

plot(hp, mpg)
par(mfrow=c(1, 2))
plot(hp[am == 0], mpg[am == 0])
plot(hp[am == 1], mpg[am == 1])
par(mfrow=c(1, 1))
