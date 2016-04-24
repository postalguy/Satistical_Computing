# Scripte pour le modele regression 

# Chargement de donnees
Migrated_Model_DATA <- read.csv("C:/Users/User/Desktop/donnee/Migrated_Model_DATA.txt", sep=";", na.strings=".")
View(Migrated_Model_DATA)
Migrated_Model_DATA$PLAN_SOURCE <- as.factor(Migrated_Model_DATA$PLAN_SOURCE)
Migrated_Model_DATA$PLAN_CIBLE <- as.factor(Migrated_Model_DATA$PLAN_CIBLE)
Migrated_Model_DATA$LIGNE <- nombreToLigne(Migrated_Model_DATA$LIGNE)
View(Migrated_Model_DATA)
#############################################################################################"
#regression binomiale de chaque categorie : 
 ## Pour Hani : la variable binaire Will_Hani si 1 la ligne a migre vers hani 
  attach(Migrated_Model_DATA)
  Migrated_Model_DATA$WILL_HANI <- ifelse(PLAN_CIBLE_CATEGO == "Pack Hany" , 1, 0)
  
 ## Pour Meditel abonement : WILL_Abo 
  Migrated_Model_DATA$WILL_ABO <- ifelse(PLAN_CIBLE_CATEGO == "Meditel Abonnement" , 1, 0)
 ## Pour Meditel Illimite : WILL_ILLIM
  Migrated_Model_DATA$WILL_ILLIM <- ifelse(PLAN_CIBLE_CATEGO == "Pack Illimite" , 1, 0)
  detach(Migrated_Model_DATA)
#############################################################################################

# Fromule : # formule de predicton 
target_Hani <- WILL_HANI ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES
target_Abo <- WILL_ABO ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES
target_Illim <- WILL_ILLIM ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES

#############################################################################################


# LOGISTIQUE
# Avec rxLogit
model_Logi_rx_Hani <- rxLogit(target_Hani, data = Migrated_Model_DATA)
model_logi_rx_Abo <- rxLogit(target_Abo, data = Migrated_Model_DATA)
model_logi_rx_Illim <- rxLogit(target_Illim, data = Migrated_Model_DATA)


# Avec rxLogit() step wise : 
variables.scope = ~ ANCIENETE + MMPR + AVG_OB_INT + AVG_OB_BASE + AVG_DATA_Go + AVG_APPELANTS + AVG_APPELES

model_Logi_rx_Step_Hani <- rxLogit(target_Hani, data = Migrated_Model_DATA,
                                   variableSelection = rxStepControl(method = "stepwise",
                                                                     scope = variables.scope))
model_logi_rx_Step_Abo <- rxLogit(target_Abo, data = Migrated_Model_DATA,
                                  variableSelection = rxStepControl(method = "stepwise",
                                                                    scope = variables.scope))
model_logi_rx_Step_Illim <- rxLogit(target_Illim, data = Migrated_Model_DATA,
                                    variableSelection = rxStepControl(method = "stepwise",
                                                                      scope = variables.scope))
################################################################################### 
# Modele lineraire general

# Modeles avec glm()
###########################
model_logi_GLM_Hani <- glm(target_Hani, family = binomial, data = Migrated_Model_DATA )
model_logi_GLM_Abo <- glm(target_Abo, family = binomial, data = Migrated_Model_DATA )
model_logi_GLM_Illim <- glm(target_Illim, family = binomial, data = Migrated_Model_DATA )

# Modeles avec rxGlm()
model_logi_GLM_Hani <- rxGlm(target_Hani, family = binomial(), data = Migrated_Model_DATA ,dropFirst = TRUE)
model_logi_GLM_Abo <- rxGlm(target_Abo, family = binomial(), data = Migrated_Model_DATA,dropFirst = TRUE )
model_logi_GLM_Illim <- rxGlm(target_Illim, family = binomial(), data = Migrated_Model_DATA ,dropFirst = TRUE)


