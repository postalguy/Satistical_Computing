# Ce scripte traite les fichiers de donnees de grande taille 
# Reposant sur l'utilisation de RevoScaleR de RRE 
# Optimise Les fonctions d'importation et d'exploration 


# 1 - Preparation de l'espace de travail 
data.dir <- "C/Users/Fsociety/Bigdatasamples"
file.Input <- file.path(data.dir,"Nom_du_fichier.csv")

# 2 - Passage du csv => xdf (RxXdfObject)
Data.DS <- RxXdfData(file.Input) 
  # Summary of the active data
  rxGetInfo(Data.DS, getVarInfo = TRUE) # getVarInfo est pour chaque variable (colonne)

# 3 - Lecture d'une partie des enregistrements 
  # varsToKeep : les colonnes a prendre 
  # startRow : la ligne ou commence le tranquage 
  # numRow : le nombre d'enregistrements a prendre 
Data.Sample <- rxReadXdf(file = Data.DS, varsToKeep = c("Var1","Var2","Var3"), startRow = 10000, numRows = 1000)


# 4 - informations globales sur l'echantillon 
rxSummary (Data.Sample)


###########################################
#Importation de fichiers separes 
#########################################
# Exemple : mortDefault2000.csv 
#			mortDefault2001.csv
#			...........2009.csv
#########################################
Data.Directory <- "C:/Users/Fsociety/Bigdatasamples"
Data.File <- file.path(Data.Directory,"mortDefault")
mortXdfFileName <- "mortDefault.xdf"

mortDataSet <- RxXdfData(mortXdfFileName)

rxSummary(~., data=mortDataSet, blocksPerRead = 2) # separation de traitement



# Filtrer une variable 
	# Facteur
	iris[iris[,"Species"]=="setosa",] # Selectionne tous les enregistrements avec Species = setosa
	#ou : - iris[iris$Species=="setosa", ] ou -  subset(iris, Species == "setosa")
	# Continue 
	subset(iris, Petal.Width < 3 & Petal.Width >= 2) # valeur entre 2 et 3
	# Mixte 
	iris[ which(iris$Petal.Width < 3 & iris$Species == "setosa") , ]


# Le Tri 

sortByVars <- c("Sepal.Length", "Sepal.Width")
decreasing <- c(TRUE, FALSE) # length decreasing and width increasing
rxSort(inData = inXDF, outFile = outXDF1, sortByVars = sortByVars,
   decreasing = decreasing) # supporte le blocksPerRead
z1 <- rxReadXdf(outXDF1)
print(head(z1,10))
