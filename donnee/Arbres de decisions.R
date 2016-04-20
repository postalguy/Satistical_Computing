#Libs 
library(splitstackshape)
library(rattle)
library(rpart)
library(party)


# Chargement 
Migrated_Model_DATA <- read.csv("C:/Users/Pr03450/Desktop/ISMAIL/donnee/Migrated_Model_DATA.txt", sep=";", na.strings=".")
View(Migrated_Model_DATA)
Migrated_Model_DATA$PLAN_SOURCE <- as.factor(Migrated_Model_DATA$PLAN_SOURCE)
Migrated_Model_DATA$PLAN_CIBLE <- as.factor(Migrated_Model_DATA$PLAN_CIBLE)
Migrated_Model_DATA$LIGNE <- nombreToLigne(Migrated_Model_DATA$LIGNE)
View(Migrated_Model_DATA)

#split data 


set.seed(1)
Donnees_Stra<- stratified(Migrated_Model_DATA, c("PLAN_CIBLE_CATEGO"),min(table(Migrated_Model_DATA$PLAN_CIBLE_CATEGO)))
ind <- sample(2, nrow(Donnees_Stra), replace=TRUE, prob=c(0.7, 0.3))
trainData <- Donnees_Stra[ind==1,]
testData <- Donnees_Stra[ind==2,]


# formule de predicton 
target <- PLAN_CIBLE_CATEGO ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES

# 1 - modèle 1 : Rpart 
Arbre_rpart <- rpart(target, data=Migrated_Model_DATA, control = rpart.control(maxdepth = 10,cp = 0.002))
plot(Arbre_rpart)
text(Arbre_rpart)

# 2 - modele : party 
Arbre_party <- ctree(target,Migrated_Model_DATA)
plot(Arbre_party, type="simple")

# 3 - modele : rxDtree
Arbre <- rxDTree(PLAN_CIBLE_CATEGO ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES,data = trainData, maxDepth = 10,cp = 0.002 )
fancyRpartPlot(rxAddInheritance(Arbre))