# 1 - Forme basique  ( Exemple sur IRIS)

summary(iris$Petal.Width)
boxplot(iris$Petal.Width)
boxplot.stats(iris$Petal.Width)$out #Affiche ces valeurs 



# 2 - Pour des données multi variables 

iris2 <- iris
attach(iris2)
# Chercher l'index des valeurs abérantes d'une variables parmis iris2 
(a <- which(Sepal.Width %in% boxplot.stats(Sepal.Width)$out)) # Affiche les indexes 
(b <- which(Sepal.Length %in% boxplot.stats(Sepal.Length)$out))
detach(iris2)

# outliers sur deux critères ( A ET B )

(outlier.list1 <- intersect(a,b))
plot(iris2$Sepal.Width,iris2$Sepal.Length)
points(df[outlier.list1,], col="red", pch="+", cex=2.5)



# outliers sur deux critères ( A OU B )

(outlier.list2 <- union(a,b))
plot(iris2$Sepal.Width,iris2$Sepal.Length)
points(df[outlier.list2,], col="blue", pch="x", cex=2)

