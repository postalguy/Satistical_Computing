# Estimer un modele lineaire a partir d'un immense set de data 

# Ce script repose sur le precedant (les variables d'environement) et donnees charges 



# pour traiter les donnees et en sortir avec un modele lineaire il faut traiter bloque par bloque 
# l'argument blocksPerRead ajuste cette valeur (equivalence entre rapidite et memoire)
# tester le temps du processus en l'incluant dans 
# system.time( { } )

varY <- rxLinMod(varY ~ varx1,
					 data = Data.DS, cube = TRUE,
					 blocksPerRead = 30)

# resume du modele 
summary(varY)


#Comparer la sortue de deux modeles 

varY2 <- rxLinMod(varY2 ~ varx1, data = Data.DS, 
					 cube = TRUE, blocksPerRead = 30)

resultatCube <- rxResultsDF(varY)
resultatCube$modelLin2 <- rxResultsDF(varY2)$varY2

 #Comparer 
 rxLinePlot (varY + varY2 ~ varx1, data = resultatCube,
 			title = " varY et varY2 par varx1" )



# Modele a variables independantes 

modelLin <- rxLinMod(varY ~ varx1 + varx2, data = Data.DS,
					cube = TRUE, blocksPerRead = 30)
modelCoef <- coef(modelLin)
cat("Nombre de coef estimes ", length(!is.na(modelCoef)))



# pour des variables factors 
modelLin <- rxLinMod(varY ~ varx1 + varx2 + F(varFact1), data = Data.DS,
					cube = TRUE, blocksPerRead = 30)


# fonction qui Estime apres le modele
expectedOutput <- function (var1, var2, var3){
	coeffNames <- c(as.character(var1),as.character(var2),as.character(var3))
	return(sum(modelCoef[coeffNames]))
}