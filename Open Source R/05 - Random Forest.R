# Random Forest est un ensemble de techniques combinees
# pour avoir un resultat 
# Il repose sur la creation de plusieurs arbres 

# Ce script assume que les informations sont importes et subdivise (Script 04)

model.rf <- randomForest(RainTomorrow ~ ., 
                         data = data[train,],
                         ntree = 500,  # Avec 500 Arbres
                         importance = TRUE,
                         na.action = na.roughfix,
                         replace= FALSE)

# Examiner le modèle 
model.rf  # Resume du model 


# 
head(model.rf$predicted)
round(head(model.rf$err.rate,15),4) # voir le taux d'erreur
min.err <- min(data.frame(model.rf$err.rate)["OOB"]) # Arbre avec le taux minimum d'erreur
min.err.index


# Lister l'importance des variables 
rn <- round(importance(model.rf),2)
rn[order(rn[,3],decreasing = TRUE),]

# visualiser l'iportance 
varImpPlot(model.rf,main = "",col="dark blue")
title(main="Importance des variables")

# Visualiser le taux d'erreur par nombre d'arbres 
plot(model.rf,main="")
legend("topright",c("OOB","No","Yes"),text.col =1:6, lty = 1:3,col=1:3)
title(main = "Taux d'erreur de foret")



################# EVALUATION DE PERFORMANCE 


# Lancer le modele sur le set d'entrainement
pr <- predict(model.rf,data[test,],type="class")

# Generation de la matrice de confusion (Affichage par effectif)
CM <- table(data[test,]$RainTomorrow,pr,dnn = c("Actuel","Predit"))
CM 

# Generation de matrice de confusion (Affichage par pourcentage)
CMpct <- round(CM/length(pr),2)
CMpct

# Calcule du pourcentage d'erreur golobal
OA <- overall(table(pr,weather[test,]$RainTomorrow,dn=c("Predit","Actuel")))
OA
x <- CM[1,2]/(CM[1,1]+CM[1,2])
y <- CM[2,1]/(CM[2,1]+CM[2,2])
p1 <- paste("Le model predit lorsqu il n y aura pas de pluie",100*round(x,3),"% du temps")
p2 <- paste("Il pleuvera ",100*round(y,3),"% du temps selon la partie sans pluie du model")
p1
p2







################ Model 2 : avec un data set d'entrainement balance ##############

model2.rf <- randomForest(RainTomorrow ~ ., 
                          data = data[train,],
                          ntree = 500,
                          mtry=4,
                          importance=TRUE,
                          na.action = na.roughfix,
                          samplesize = c(35,35), # echantillonage balance
                          replace=FALSE)
model2.rf

