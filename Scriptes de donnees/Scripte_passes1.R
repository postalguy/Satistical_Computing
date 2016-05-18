Donnees_Plus_Passe <- read.csv("C:/Users/User/Desktop/donnee/Donnees_modele_usage_etoile.csv", sep=";", na.strings=".")
Donnees_Plus_Passe$LIGNE <- as.character(paste("0",Donnees_Plus_Passe$LIGNE,sep = ""))
Donnees_Plus_Passe$PLAN_SOURCE <- as.factor(as.character(Donnees_Plus_Passe$PLAN_SOURCE))
Donnees_Plus_Passe$PLAN_CIBLE <- as.factor(as.character(Donnees_Plus_Passe$PLAN_CIBLE))
for (i in 14:length(names(Donnees_Plus_Passe))) {
  Donnees_Plus_Passe[,i] <- ifelse(is.na(Donnees_Plus_Passe[,i]),0,Donnees_Plus_Passe[,i])
}

Donnees_Plus_Passe$ca_pass_3_2016 <- NULL
Donnees_Plus_Passe$NB_pass_3_2016 <- NULL
Donnees_Plus_Passe$ca_pass_4_2016 <- NULL
Donnees_Plus_Passe$NB_pass_4_2016 <- NULL

attach(Donnees_Plus_Passe)
# Pour des raisons de calcule 
for (i in seq(15,41,by = 2)) {
  Donnees_Plus_Passe[,i] <- ifelse(Donnees_Plus_Passe[,i]==0 , 1 , Donnees_Plus_Passe[,i] )
}

Donnees_Plus_Passe$AVG_passes_1_2016 <- ca_pass_1_2016 / NB_pass__1_2016
Donnees_Plus_Passe$AVG_passes_2_2016 <- ca_pass_2_2016 / NB_pass_2_2016
Donnees_Plus_Passe$AVG_passes_1_2015 <- ca_pass_01_2015 / NB_pass_01_2015
Donnees_Plus_Passe$AVG_passes_2_2015 <- ca_pass_02_2015 / NB_pass_02_2015
Donnees_Plus_Passe$AVG_passes_3_2015 <- ca_pass_03_2015 / NB_pass_03_2015
Donnees_Plus_Passe$AVG_passes_4_2015 <- ca_pass_04_2015 / NB_pass_04_2015
Donnees_Plus_Passe$AVG_passes_5_2015 <- ca_pass_05_2015 / NB_pass_05_2015
Donnees_Plus_Passe$AVG_passes_6_2015 <- ca_pass_06_2015 / NB_pass_06_2015
Donnees_Plus_Passe$AVG_passes_7_2015 <- ca_pass_07_2015 / NB_pass_07_2015
Donnees_Plus_Passe$AVG_passes_8_2015 <- ca_pass_08_2015 / NB_pass_08_2015
Donnees_Plus_Passe$AVG_passes_9_2015 <- ca_pass_09_2015 / NB_pass_09_2015
Donnees_Plus_Passe$AVG_passes_10_2015 <- ca_pass_10_2015  / NB_pass_10_2015
Donnees_Plus_Passe$AVG_passes_11_2015 <- ca_pass_11_2015 / NB_pass_11_2015
Donnees_Plus_Passe$AVG_passes_12_2015 <- ca_pass_12_2015 / NB_pass_12_2015
for (i in 42:55) {
  Donnees_Plus_Passe[,i] <- ifelse(is.nan(Donnees_Plus_Passe[,i]) , 0 , Donnees_Plus_Passe[,i] )
}
detach(Donnees_Plus_Passe)
attach(Donnees_Plus_Passe)
Donnees_Plus_Passe$AVG_PASSE1_MENS <- (   AVG_passes_1_2016+
                                          AVG_passes_2_2016+
                                          AVG_passes_1_2015+
										                      AVG_passes_2_2015+
										                      AVG_passes_3_2015+
										                      AVG_passes_4_2015+
										                      AVG_passes_5_2015+
										                      AVG_passes_6_2015+
										                      AVG_passes_7_2015+
										                      AVG_passes_8_2015+
										                      AVG_passes_9_2015+
										                      AVG_passes_10_2015+
										                      AVG_passes_11_2015+
										                      AVG_passes_12_2015)/6
detach(Donnees_Plus_Passe)
Donnees_Plus_Passe1 <- Donnees_Plus_Passe[,-(14:55)]
rm(Donnees_Plus_Passe)
rm(i)
Donnees_Plus_Passe1$F1 <- NULL


