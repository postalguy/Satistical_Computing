library(randomForest)

############# MISSING VALUES ALERT !!!!!!!!!!!!!!!!
# # Chargement 
# Donnees_Plus_Passe1 <- read.csv("C:/Users/User/Desktop/donnee/Donnees_Plus_Passe1.txt", sep=";", na.strings=".")
# View(Donnees_Plus_Passe1)
# Donnees_Plus_Passe1$PLAN_SOURCE <- as.factor(Donnees_Plus_Passe1$PLAN_SOURCE)
# Donnees_Plus_Passe1$PLAN_CIBLE <- as.factor(Donnees_Plus_Passe1$PLAN_CIBLE)
# Donnees_Plus_Passe1$LIGNE <- nombreToLigne(Donnees_Plus_Passe1$LIGNE)
# Donnees_Plus_Passe1 <- Donnees_Plus_Passe1[complete.cases(Donnees_Plus_Passe1),]
# View(Donnees_Plus_Passe1)

# CHARGEMENT NOUVEAU : 
####
#### EXECUTER LE SCRIPTE Scripte_passes1.R en entier
####

#split data 


set.seed(1)
Donnees_Stra<- stratified(Donnees_Plus_Passe1, c("PLAN_CIBLE_CATEGO"),3000)
ind <- sample(2, nrow(Donnees_Stra), replace=TRUE, prob=c(0.7, 0.3))
trainData <- Donnees_Stra[ind==1,]
testData <- Donnees_Stra[ind==2,]



# formule de predicton 
target <- PLAN_CIBLE_CATEGO ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES+ AVG_PASSE1_MENS

randForest <- randomForest(target,data = trainData, ntree=100 , proximity = TRUE)