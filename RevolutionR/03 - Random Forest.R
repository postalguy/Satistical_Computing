# Random Forest est un ensemble de techniques combinees
# pour avoir un resultat 
# Il repose sur la creation de plusieurs arbres 

# Ce script utilise des fonctions de RRE pour gérer les données massives 

# Separation de la table en donnees de test et d'apprentissage 
set.seed(1234)
 ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
 trainData <- iris[ind==1,]
 testData <- iris[ind==2,]
 
 

irisForest <- rxDForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
 seed = 10,
data = iris, cp=0.01, nTree=500, mTry=3, blocksPerRead = 2)
irisForest


predictedSpecies <- rxPredict(irisForest, data=iris)
