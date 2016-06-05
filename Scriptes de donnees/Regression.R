# Scripte pour le modele regression 

# Chargement de donnees
# Donnees_Plus_Passe1 <- read.csv("C:/Users/User/Desktop/donnee/Donnees_Plus_Passe1.txt", sep=";", na.strings=".")
# View(Donnees_Plus_Passe1)
# Donnees_Plus_Passe1$PLAN_SOURCE <- as.factor(Donnees_Plus_Passe1$PLAN_SOURCE)
# Donnees_Plus_Passe1$PLAN_CIBLE <- as.factor(Donnees_Plus_Passe1$PLAN_CIBLE)
# Donnees_Plus_Passe1$LIGNE <- nombreToLigne(Donnees_Plus_Passe1$LIGNE)
# Donnees_Plus_Passe1 <- Donnees_Plus_Passe1[complete.cases(Donnees_Plus_Passe1),]
# View(Donnees_Plus_Passe1)

# CHARGEMENT NOUVEAU : 
####
#### EXECUTER LE SCRIPTE Scripte_passes1.R en entier
####









#############################################################################################"
#regression binomiale de chaque categorie : ONE VS ALL RULE
 ## Pour Hani : la variable binaire Will_Hani si 1 la ligne a migre vers hani 
  attach(Donnees_Plus_Passe1)
  Donnees_Plus_Passe1$WILL_HANI <- ifelse(PLAN_CIBLE_CATEGO == "Pack Hany" , 1, 0)
  
 ## Pour Meditel abonement : WILL_Abo 
  Donnees_Plus_Passe1$WILL_ABO <- ifelse(PLAN_CIBLE_CATEGO == "Meditel Abonnement" , 1, 0)
 ## Pour Meditel Illimite : WILL_ILLIM
  Donnees_Plus_Passe1$WILL_ILLIM <- ifelse(PLAN_CIBLE_CATEGO == "Pack Illimite" , 1, 0)
  detach(Donnees_Plus_Passe1)
#############################################################################################

# Fromule : # formule de predicton 
target_Hani <- WILL_HANI ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES + AVG_PASSE1_MENS
target_Abo <- WILL_ABO ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES + AVG_PASSE1_MENS
target_Illim <- WILL_ILLIM ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES + AVG_PASSE1_MENS

#############################################################################################


# LOGISTIQUE
# Avec rxLogit
model_Logi_rx_Hani <- rxLogit(target_Hani, data = Donnees_Plus_Passe1)
model_logi_rx_Abo <- rxLogit(target_Abo, data = Donnees_Plus_Passe1)
model_logi_rx_Illim <- rxLogit(target_Illim, data = Donnees_Plus_Passe1)


# Avec rxLogit() step wise : 
variables.scope = ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES + AVG_PASSE1_MENS

model_Logi_rx_Step_Hani <- rxLogit(target_Hani, data = Donnees_Plus_Passe1,
                                   variableSelection = rxStepControl(method = "stepwise",
                                                                     scope = variables.scope))
model_logi_rx_Step_Abo <- rxLogit(target_Abo, data = Donnees_Plus_Passe1,
                                  variableSelection = rxStepControl(method = "stepwise",
                                                                    scope = variables.scope))
model_logi_rx_Step_Illim <- rxLogit(target_Illim, data = Donnees_Plus_Passe1,
                                    variableSelection = rxStepControl(method = "stepwise",
                                                                      scope = variables.scope))
################################################################################### 
# Modele lineraire general

# Modeles avec glm()
###########################
model_logi_GLM_Hani <- glm(target_Hani, family = binomial, data = Donnees_Plus_Passe1 )
model_logi_GLM_Abo <- glm(target_Abo, family = binomial, data = Donnees_Plus_Passe1 )
model_logi_GLM_Illim <- glm(target_Illim, family = binomial, data = Donnees_Plus_Passe1 )

# Modeles avec rxGlm()
model_logi_rxGLM_Hani <- rxGlm(target_Hani, family = binomial(), data = Donnees_Plus_Passe1 ,dropFirst = TRUE)
model_logi_rxGLM_Abo <- rxGlm(target_Abo, family = binomial(), data = Donnees_Plus_Passe1,dropFirst = TRUE )
model_logi_rxGLM_Illim <- rxGlm(target_Illim, family = binomial(), data = Donnees_Plus_Passe1 ,dropFirst = TRUE)


##################### Scoring et evaluation 

#  Avec rxLogit
print(model_logi_rx_Abo)
print(model_logi_rx_Illim)
print(model_Logi_rx_Hani)

# GLM 
print(model_logi_GLM_Hani)
print(model_logi_GLM_Abo )
print(model_logi_GLM_Illim)