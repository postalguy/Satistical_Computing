# Transformer les donnees pour les preparer pour un traitement 
# exemple sur un dataset de sorte 

# Sex	age	Height	Pounds	Sport
# male	36	5ft4in	140.563	tennis
#female	24	....

xform <- function(datalist){
	# Creer une variable continue d'une autre continue existante 
	datalist$kilos <- datalist$Pounds * 0.4535
	datalist$Pounds <- NULL  #Supprimer l'ancienne colonne

	#Creer une variable continue d'un facteur 
	HeightStr <- as.character(datalist$Height) #Parser en chaine de caracteres 
	feet <- as.numeric(sub("ft","",HeightStr)) # Lire la hauteur en ft 
	inches <- as.numeric(sub("in","",sub(".*ft","",HeightStr))) #Lire la hauteur en inches
	datalist$HeightInches <- feet*12 + inches
	#trie d'un facteur 
	datalist$Sport <- factor(datalist$Sport, levels= sort(levels(datalist$Sport)))
	datalist
}	