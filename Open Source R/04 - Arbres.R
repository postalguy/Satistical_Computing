#Script de partitionnement recursive par arbres
#Partitionne selon un critere de facon recursive
#La meilleure facon de partitionner  : 
# selon le gain et Gini (deux algos)

# Ce scripte depend du cahrgement du precedant (setup & explorer)


# Selection des variables du modele 
data <- subset(weather,select=c(MinTemp:RainToday,RainTomorrow))


# Cree une subdivision pour l'apprentissage 
set.seed(42) #seed for random
N <- nrow(data)
train <- sample(N,0.7*N) # choisit pour l'apprentissage les indices de lignes.
test <- setdiff(setdiff(seq_len(N),train),train)


#-------- Construction du modele arbre
form <- formula(RainTomorrow ~ .) # Formule du modèle
model <- rpart(formula=form , data= data[train,]) 

# Visualiser l'arbre 
drawTreeNodes(model) # Prise de Rattle
title(main="Arbre de decision")

# - 3 méthodes pour examiner les resultats

# str(model)
# model
# printcp(model)  #depuis rpart
summary(model)

#-------------------
# Savoir sur quel feuille chaque observation a finie
leaf <- model$where
leaf

#----------- evaluer la performance 
# lancer le modèle arbre sur les informations valides 
pr <- predict(model, weather[test,],type ="class")
# Generer une matrice de confusion 
AP <- c("actuel","predit")
CM <- table(weather[test,]$RainTomorrow,pr,dnn=AP)
CMcpt <- round(CM/length(pr),2)

# Une foction pour calculer les erreurs 
overall <- function(x){
	if(nrow(x)==2)
		oe <- (x[1,2]+x[2,1]) / sum(x)
	else 
		oe <- 1 - (x[1,2]+x[2,1]) / sum(x)
	return (oe)
}


oe <- overall(CM) # Toutes les erreures
CM;CMcpt;oe


# Savoir la performance avec la courbe ROC 

   # Prendre le vecteur RainTomorrow dans un data set 
   RT <- weather[test,]$RainTomorrow
   prRT <- as.vector(as.integer(pr))
   pred <- prediction(predictions= prRT, labels = RT)  #Prediction fait partie de ROCR
   # Visualiser la courbe ROC
   plot(performance(pred,"tpr","fpr"),col="#CC0000FF",lty=1,lwd=2,add=FALSE)
   segments(0,0,1,1,col="blue",lwd=2)
   legend("bottomright",c("tree.m"),col=rainbow(1,1,.8),lty=1:1 , title="Modeles",inset = c(0.5,0.5))
   title(main="Courbe ROC")
   

