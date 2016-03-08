# Reading and exploring 
#dépendance de quelques blocs du script précédant
#-----------------------------
# 1 - Chargement de fichier 
#-----------------------------
name <- "weather.csv"
DataDir <- "C:/Users/Fsociety/Desktop/BI MEDITEL/DataminingScripts/Data"
file <- file.path(DataDir,name)
weather <- read.csv(file)

#-----------------------------
# 2 - Substitution de quelques colonnes 
#----------------------------
weather <- weather[,-c(1,2)] #1ere et 2eme colonnes supprimees


#-------------------------------
# 3 - Avoir un compte rendu de toutes les variables
#----------------------------------
summary(weather)
#ou
describe(weather)



#------------------------------
# 4 - Savoir la structure des données 
#------------------------------
str(weather)



#---------------------------------
# 5 - Visualisation d'une seule variable 
#---------------------------------
ds <- summary(weather$RainTomorrow) #resume de la variable
# ploter en barplot
bp <- barplot2(ds,beside=TRUE,xlab="Rain Tomorrow",ylab="Frequence",ylim=c(0,max(ds)+15),col="blue") # xlab ="mettre le label"

# afficher les fréquence de chaque valeur 
text(bp,ds+9,ds)

# si �a donne une erreur ajouter la fonction : 
resize.win <- function(Width=6, Height=6)
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}

# Puis appeler > resize.win(100,100)






#-----------------------------------
# 6 - Visualiser la distribution de quelques variables
#-----------------------------------
hs <- hist(weather$Sunshine,
		xlab="Sunshine", ylab="Fréquence",
		col= "grey90", ylim=c(0,90),
		breaks="fd", border=TRUE)
dens <- density(weather$Sunshine, na.rm=TRUE)
rs <- max(hs$counts)/max(dens$y)
lines(dens$x,dens$y*rs, type="l",col=rainbow_hcl(1)[1])
rug(weather$Sunshine) 
title(main=paste("Distribution de","Sunshine")) # Ajoute le titre

#----- Fonction pour plusieurs Histogrames
jplot <- function(var){
	hs<- hist(eval(parse(text=var)),main="",xlab=var,ylab="Fréquence",
		col="grey90", ylim=c(0,90),breaks="fd",border=TRUE)
	dens <- density(eval(parse(text=var)), na.rm=TRUE)
	rs <- max(hs$counts)/max(dens$y)
	lines(dens$x,dens$y*rs,type="l",col=rainbow_hcl(1)[1])
	rug(eval(parse(text=var)))
	title(main=paste("Distribution de ",var))
}


#---- Visualiser 4 plots des variables du dataset
attach(weather) # Ceci rendera les variables du dataset disponibles
# Visualiser les distributions avec jplot
par(mfrow=c(2,2))
v<-c("Sunshine","WindGustSpeed","WindSpeed9am","WindSpeed3pm")
lapply(v,jplot)
detach(weather)


#--------------------------------
# 7 - Visualiser la corrélation
#-------------------------------
numeric <- c("MinTemp","MaxTemp","Rainfall","Evaporation",
			"Sunshine","WindGustSpeed","WindSpeed9am","WindSpeed3pm",
			"Humidity9am","Humidity3pm","Pressure9am","Pressure3pm",
			"Cloud9am","Cloud3pm","Temp9am","Temp3pm")
#
cor <- cor(weather[,numeric],use="pairwise",method="pearson")
# affichage
par(mfrow=c(1,1))
plotcorr(cor,col=colorRampPalette(c("red","white","blue"))(11)[5*cor + 6])
title(main="Correlation en utilisant Pearson", sup=paste(format(Sys.time(),"%Y-%d-%b %H:%M:%S"),Sys.info()["user"]))


