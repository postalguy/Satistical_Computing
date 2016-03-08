# Script de prediction via arbre de decision via rxDTree 



# Separation de la table en donnees de test et d'apprentissage 
set.seed(1234)
 ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
 trainData <- iris[ind==1,]
 testData <- iris[ind==2,]
 
 
 # Travail avec RevoScaler R 
  RevoTree <- rxDTree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
 # Prediction 
  rxPredict(RevoTree , data = trainData)


  ########### Travail sur des donnes larges #################
# maxDepth :
	#cela definit la profondeur maximale de n'importe quel nœud de l'arbre. 
	#Le calcule s'agrandit rapidement des que la profondeur augmente,
	#un maxDepth de 10 à 15 est pas mal.
# cp : 
	#ceci est un parametre de complexite et met la barre pour combien une split doit reduire la la complexite avant d'etre accepte.
	#Si vous souhaitez specifier une valeur de cp, commencer par une valeur prudente,
	#comme rpart 0,01 ensuite diminuer le cp par des puissances de 10
	

    RevoTree <- rxDTree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
    		 data = iris,
    		 maxDepth = 5, 
    		 cp = 1e-05, 
    		 blocksPerRead = 30)