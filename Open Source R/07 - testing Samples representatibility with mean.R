# IMPORTATION : 
DMRT <- read_csv("C:/Users/Ismail AKRIM/Desktop/dataart_sem_ech.csv")

# Spliting des données : 

set.seed(1)
ind <- sample(2, nrow(DMRT), replace=TRUE, prob=c(0.7, 0.3))
trainData <- DMRT[ind==1,]
testData <- DMRT[ind==2,]

attach(DMRT)
attach(testData)
attach(trainData)
PV <- list()
nam <- list()
for (i in 2:length(names(DMRT))) {
 PV[[i]] <-  t.test(as.data.frame(trainData[i]),as.data.frame(testData[i]))$p.value
 nam[[i]] <- names(trainData)[i]
}
View(as.data.frame(cbind(nam,PV)))

