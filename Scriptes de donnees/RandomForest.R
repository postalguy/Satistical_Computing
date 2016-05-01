library(randomForest)

############# MISSING VALUES ALERT !!!!!!!!!!!!!!!!
# Chargement 
Migrated_Model_DATA <- read.csv("C:/Users/User/Desktop/donnee/Migrated_Model_DATA.txt", sep=";", na.strings=".")
View(Migrated_Model_DATA)
Migrated_Model_DATA$PLAN_SOURCE <- as.factor(Migrated_Model_DATA$PLAN_SOURCE)
Migrated_Model_DATA$PLAN_CIBLE <- as.factor(Migrated_Model_DATA$PLAN_CIBLE)
Migrated_Model_DATA$LIGNE <- nombreToLigne(Migrated_Model_DATA$LIGNE)
Migrated_Model_DATA <- Migrated_Model_DATA[complete.cases(Migrated_Model_DATA),]
View(Migrated_Model_DATA)


#split data 


set.seed(1)
Donnees_Stra<- stratified(Migrated_Model_DATA, c("PLAN_CIBLE_CATEGO"),3000))
ind <- sample(2, nrow(Donnees_Stra), replace=TRUE, prob=c(0.7, 0.3))
trainData <- Donnees_Stra[ind==1,]
testData <- Donnees_Stra[ind==2,]



# formule de predicton 
target <- PLAN_CIBLE_CATEGO ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES

randForest <- randomForest(target,data = trainData, ntree=10 , proximity = TRUE)