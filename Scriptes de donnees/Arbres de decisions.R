#Libs 
library(splitstackshape)
library(rattle)
library(rpart)
library(party)

#########################################################################################
# Chargement 
# Donnee_Plus_Passe1 <- read.csv("C:/Users/User/Desktop/donnee/Donnee_Plus_Passe1.txt", sep=";", na.strings=".")
# Donnee_Plus_Passe1$PLAN_SOURCE <- as.factor(Donnee_Plus_Passe1$PLAN_SOURCE)
# Donnee_Plus_Passe1$PLAN_CIBLE <- as.factor(Donnee_Plus_Passe1$PLAN_CIBLE)
# Donnee_Plus_Passe1$LIGNE <- nombreToLigne(Donnee_Plus_Passe1$LIGNE)
# Donnee_Plus_Passe1 <- Donnee_Plus_Passe1[complete.cases(Donnee_Plus_Passe1),]
# View(Donnee_Plus_Passe1)

# CHARGEMENT NOUVEAU : 
####
#### EXECUTER LE SCRIPTE Scripte_passes1.R en entier
####


###########################################################################################
#split data 
set.seed(1)
Donnees_Stra<- stratified(Donnees_Plus_Passe1, c("PLAN_CIBLE_CATEGO"),2000)
ind <- sample(2, nrow(Donnees_Stra), replace=TRUE, prob=c(0.7, 0.3))
trainData <- Donnees_Stra[ind==1,]
testData <- Donnees_Stra[ind==2,]

###########################################################################################
# formule de predicton 
target <- PLAN_CIBLE_CATEGO ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES + AVG_PASSE1_MENS

###########################################################################################
# 1 - modèle 1 : Rpart 
Arbre_rpart <- rpart(target, data=trainData,method = "class", control = rpart.control(maxdepth = 20,cp = 0.005),parms = list(split = 'information'))
fancyRpartPlot(Arbre_rpart)

###########################################################################################
# 2 - modele : party  
  Arbre_party <- ctree(target,trainData, controls = ctree_control(maxdepth = 10,))
plot(Arbre_party, type="simple")

###########################################################################################
# 3 - modele : rxDtree
Arbre <- rxDTree(PLAN_CIBLE_CATEGO ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES + AVG_PASSE1_MENS,data = trainData, maxDepth = 10,cp = 0.05 )
fancyRpartPlot(rxAddInheritance(Arbre))


######################## 
#PRUNING TREES 
#Valoriser l'erreur / taille de l'arbre 

plotcp(Arbre_rpart)
plotcp(Arbre_party)
plotcp(rxAddInheritance(Arbre))

######### Rpart 
prediction_rpart <- predict(Arbre_rpart, type = "class")
tb_rpart <- table(prediction_rpart,trainData$PLAN_CIBLE_CATEGO)
erreur_rpart <- 1-sum(diag(tb_rpart))/sum(tb_rpart)
print(erreur_rpart)
######### Party
prediction_party <- predict(Arbre_party)
tb_party <- table(prediction_party,trainData$PLAN_CIBLE_CATEGO)
erreur_party <- 1-sum(diag(tb_party))/sum(tb_party)
print(erreur_party)
######### rxDtree