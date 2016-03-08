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

append <- "none"
for (i in 2000:2009) {
	importFile <- paste("mortDefault",i,".csv",sep="")
	mortDataSet <- rxImport(importFile,mortXdfFileName,append=append)
	append <- "rows"
}
mortDataSet <- RxXdfData(mortXdfFileName)

rxSummary(~., data=mortDataSet, blocksPerRead = 2) # separation de traitement

