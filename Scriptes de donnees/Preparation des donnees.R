########################## Script ##############

#------ Comment utiliser ? --------
# 1 - importer la table : > avril <- importDMRT_MMXX("DMRT_AVRT15.txt")
# 2 - appliquer la transformation : > avril <- data_model(avril)





#######################Fonctions############################
# Rassembler ce processus sous une fonction 
# importDMRT_MMXX() va permettre d'importer 

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C") 
importDMRT_MMXX <- function(filefulln){
  
  file_path <- paste(as.character(paste(getwd(),'/',sep = "")),filefulln,sep="")
  filen <- read.csv(file_path, sep=";", na.strings=".")
  
  # rendre le code de l'offre comme facteur 
  filen$PLAN_SOURCE <- as.factor(as.character(filen$PLAN_SOURCE))
  filen$PLAN_CIBLE <- as.factor(as.character(filen$PLAN_CIBLE))
  # Ajouter le 0 pr la notion numero
  filen$LIGNE <- as.character(paste("0",filen$LIGNE,sep = ""))
  
  #Convertir les dates en format dates de R 
  # Pour avoir la date YYYY - MM - DD
  filen$DATE_ACTIVATION <-  as.character(filen$DATE_ACTIVATION)
  filen$DATE_ACTIVATION <- as.Date(as.POSIXct(filen$DATE_ACTIVATION , format='%d%B%y:%H:%M:%S'))
  filen$DATE_MIGRATION <-  as.character(filen$DATE_MIGRATION)
  filen$DATE_MIGRATION <- as.Date(as.POSIXct(filen$DATE_MIGRATION , format='%d%B%y:%H:%M:%S'))
  # Pour avoir l'heure 
  #format(as.POSIXct(DATE_ACTIVATION, format='%d%b%y:%H:%M:%S'),"%H:%M:%S")
  #format(as.POSIXct(DATE_MIGRATION, format='%d%b%y:%H:%M:%S'),"%H:%M:%S")
  print(paste("Entrer > View(","nom de la table",") pour visulaliser"))
  return(nonDuplicate(filen))
}

## Fonction d'extraction des categories de migration 
categories_Migration <- function(dataname){
  categories <- unique(dataname[,c(7,8)])
  categories <- categories[order(categories$PLAN_CIBLE),]
  return (categories)
}


# Fonction de creation de la table calculee du modele
data_model <- function(dataname){
  attach(dataname)
  MMPR <-virg((RECH_MNT_1 + RECH_MNT_2 +RECH_MNT_3 +RECH_MNT_4+ RECH_MNT_5 + RECH_MNT_6)/ (RECH_NBRE_1+RECH_NBRE_2+RECH_NBRE_3+RECH_NBRE_4+RECH_NBRE_5+RECH_NBRE_6))
  AVG_OB_INT <- virg((OB_CALL_INTL_DUR_1+OB_CALL_INTL_DUR_2+OB_CALL_INTL_DUR_3+OB_CALL_INTL_DUR_4+OB_CALL_INTL_DUR_5+OB_CALL_INTL_DUR_6)/(OB_CALL_INTL_CNT_1+OB_CALL_INTL_CNT_2+OB_CALL_INTL_CNT_3+OB_CALL_INTL_CNT_4+OB_CALL_INTL_CNT_5+OB_CALL_INTL_CNT_6))
  AVG_OB_INT <- ifelse(is.nan(AVG_OB_INT),0,AVG_OB_INT)
  AVG_OB_BASE <- virg((OB_CALL_DUR_BASE_1+OB_CALL_DUR_BASE_2+OB_CALL_DUR_BASE_3+OB_CALL_DUR_BASE_4+OB_CALL_DUR_BASE_5+OB_CALL_DUR_BASE_6)/(OB_CALL_CNT_BASE_1+ OB_CALL_CNT_BASE_2+OB_CALL_CNT_BASE_3+OB_CALL_CNT_BASE_4+OB_CALL_CNT_BASE_5+OB_CALL_CNT_BASE_6))
  AVG_DATA <- virg((VOLUME_TOTAL_NAVIGATION_1+VOLUME_TOTAL_NAVIGATION_2+VOLUME_TOTAL_NAVIGATION_3+VOLUME_TOTAL_NAVIGATION_4+VOLUME_TOTAL_NAVIGATION_5+VOLUME_TOTAL_NAVIGATION_6)/6,4)
  AVG_APPELANTS <- virg((NBRE_DISTINCT_APPELANTS_1+NBRE_DISTINCT_APPELANTS_2+NBRE_DISTINCT_APPELANTS_3+NBRE_DISTINCT_APPELANTS_4+NBRE_DISTINCT_APPELANTS_5+NBRE_DISTINCT_APPELANTS_6)/6)
  AVG_APPELES <- virg((NBRE_DISTINCT_APPELES_1+NBRE_DISTINCT_APPELES_2+NBRE_DISTINCT_APPELES_3+NBRE_DISTINCT_APPELES_4+NBRE_DISTINCT_APPELES_5+NBRE_DISTINCT_APPELES_6)/6)
  # Categoriser les plans 
  hani<- c("417","418","408","517")
  Illimite <- c("462","464","463","465","520","518","457","458","459")
  Meditel_Abon <- c("359","421","425","420","360","385","361","423","362","387","365")
  PLAN_CIBLE_CATEGO <- ifelse(PLAN_CIBLE %in% hani , "Pack Hany", ifelse(PLAN_CIBLE %in% Illimite, "Pack Illimite",
                                                                         ifelse( PLAN_CIBLE %in% Forfait_H, "delete", ifelse(PLAN_CIBLE %in% Meditel_Abon, "Meditel Abonnement", "delete"))))
  
  data.out <- data.frame(LIGNE = LIGNE,
                         ANCIENETE = as.numeric(Sys.Date()-DATE_ACTIVATION),
                         MIG_DATE = DATE_MIGRATION,
                         PLAN_SOURCE = PLAN_SOURCE,
                         PLAN_CIBLE = PLAN_CIBLE,  MMPR,
                         AVG_OB_INT,
                         AVG_OB_BASE,
                         AVG_DATA_Go = AVG_DATA/(1024*1024*1024), #'EN Go
                         AVG_APPELANTS,
                         AVG_APPELES,  PLAN_CIBLE_CATEGO)
  rm(MMPR)
  rm(AVG_OB_INT)
  rm(AVG_OB_BASE)
  rm(AVG_DATA)
  rm(AVG_APPELES)
  rm(AVG_APPELANTS)
  
  detach(dataname)
  data.out <- (data.out[data.out$PLAN_CIBLE_CATEGO != "delete",])
  return (removeNA(data.out))
}



#### UTILS 

### Fonction removeNA() elimine les NA MMPR du datamart calculé 
removeNA <- function(dataname){
  return((dataname[!is.na(dataname$MMPR),]))
}

getNA <- function(dataname){
  return((dataname[is.na(dataname$MMPR),]))
}

Completevars <- function(dataname) {
  return(dataname[complete.cases(dataname),])
}


#### Fonction nonDuplicate(dataname, column)
nonDuplicate <- function(dataname){
  return (dataname[!duplicated(dataname$LIGNE), ])
}

### fonction de gestion virgules
virg <- function(x, d= 2) {
  trunc(x*10^d)/10^d
}

# Fonction nombre to ligne 
nombreToLigne <- function(vector){
  vector <- as.character(paste("0",vector,sep = ""))
}




