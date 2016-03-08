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

