# Clustering sur R
#dépendance de quelques blocs du script précédant 

##############################################
##################### KMeans #################
##############################################
# Kmeans n'opère que sur les variables numériques 
# on fait l'extraction des données numériques dans un data frame appart
numvars <- lapply(weather,is.numeric)
numdata <- na.omit(weather[,numvars==TRUE])
head(numdata) # comment ce data frame va apparaitre View(head(numdata))

 # Lancement du kmeans algo 
 km <- kmeans(x=numdata, centers=10) # Chois de centres est 10 

 # visualiser les 5 première vaiables colorés par Cluster
 vars <- 1:5
 plot(numdata[vars],col=km$cluster)
 title(main="Weather")



 ################################################
 ########### Clustering hiérarchique ############
 ################################################

 cc <- cor(numdata, use="pairwise", method="pearson") # calcule la matrice de distances
 hc <- hclust(dist(cc),method="average") # Démare le Clustering
 
 #visualisation basique 
 dn <- as.dendrogram(hc)
 # plot(dn,horiz=TRUE)

 # Visualisation 
 op <- par(mar=c(3,4,3,4.29))
 plot(dn,horiz=TRUE,nodePar=list(col=3:2,cex = c(2.0,7.75),
 	pch=21:22,bg=c("light blue","pink"),lab.cex=0.75,lab.col ="tomato"), edgePar= list(col="gray",lwd=2), xlab="Height")
 title(main="Correlation cluster avec Pearson")
 par(op)
 

