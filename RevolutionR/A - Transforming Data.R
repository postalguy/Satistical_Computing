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


# Creer une variable sous une condition 
# pour avoir 
#x   y  w
#1   1  "good"
#2   2   "fair"
#5   5   "bad"
x <- c(1, 2, 4)
y <- c(1, 4, 5)
w <- ifelse(x <= 1, "good", ifelse((x >= 3) & (x <= 5), "bad", "fair"))
data.frame(x, y, w)



# Stratisfaction de données : 
library(splitstackshape)
set.seed(1)
out <- stratified(iris, c("Species"), 30) # 30 enregistrement de chaque espèce 