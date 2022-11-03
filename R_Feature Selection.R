



#######################################################################


# FEATURE SELECTION:

library(MASS)
library(tidyverse)
library(sf)
library(tmap)
library(dplyr)

library(gridExtra)
library(raster)
library(rasterVis)
library(rcartocolor)
library(ggplot2)
library(e1071)
library(corrplot)
library(caret)
library(randomForest)
library(FSelector)


# Excel: Daten_1_extra
# alle Einzelvariablen (nicht aggregiert) + Scores 

# Import des Excel-Files: final_data_2
# enthält den zu untersuchenden Datensatz:



final_data_relevant_1

is.na(final_data_relevant_1)<-sapply(final_data_relevant_1, is.infinite)
final_data_relevant_1[is.na(final_data_relevant_1)]<-0

final_data_relevant_1$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1$FRAUD_1_NONFRAUD_0)

final_data_relevant_1 <- as.data.frame(final_data_relevant_1)


# Untersuchung der Daten auf Multikollinearität

# https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57
corr_simple <- function(data=final_data_relevant_1,sig=0.8){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ", method="pie")
}

corr_simple()


# Entfernen der Variablen mit hoher Korrelation


final_data_relevant_1_v1<-final_data_relevant_1[, !names(final_data_relevant_1) %in% 
                                                  c("J_var2_J", "J_var2_VJ", "J_var2_4",
                                                    "ACCR_BS_J", "ACCR_BS_VJ", "ACCR_BS_4", 
                                                    "issue_X", "issue_X1", "issue_X2",
                                                    "CR_J", "CR_VJ", "CR_4",
                                                    "ASSET_TR_J", "ASSET_TR_VJ", "ASSET_TR_4",
                                                    
                                                    
                                                    
                                                    
                                                  )] 



#####################
# Feature Selection


#######################
# Random Forest

library(FSelector)
weight = random.forest.importance(FRAUD_1_NONFRAUD_0~., final_data_relevant_1,
                                  importance.type = 1)
weight
cutoff.k(weight, 10)

weight = random.forest.importance(FRAUD_1_NONFRAUD_0~., final_data_relevant_1_v1,
                                  importance.type = 1)
weight
cutoff.k(weight, 20)


colnames(final_data_relevant_1)


####################################
# Feature Selection  --> Logistische Regression jeder einzelnen Variable:
####################################

# Entfernen alles von Jones-Modell, was in MJ schon enthalten ist - also J_var2_...
# CR

# Loop durch alle Variablen - Test jeder einzelnen Variable auf Signifikanz:
# speichern in Vector a_vec:

# final_data_relevant_1_v1
final_data_relevant_1[ , 12]
final_data_relevant_1$CR_J

a_vec<-c()
y<-final_data_relevant_1$FRAUD_1_NONFRAUD_0
for(i in 2:ncol(final_data_relevant_1)) {       # for-loop over columns
  x <- final_data_relevant_1[ , i]
  
  model <- glm(y ~x, family = binomial)
  
  # in Summary ist 12 Element - Koeffizienten, p-Wert ...
  # 8. Element in Liste ist der p-Wert für untersuchte Variable
  a<-summary(model)[[12]][8]
  a_vec<-c(a_vec, a)
}


a_vec[11]

# Erhalte Index - wo p-Werte Kriterien erfüllen
significant_var<-which(a_vec < 0.1 & a_vec >0.05)
significant_var<-which(a_vec < 0.05)

# +1 da Fraud-Y-Variable im Datensatz nicht untersucht wurde
# so erhalte ich richtigen Index
significant_var<-significant_var +1
# < 0.05
# 45 52 64 65 66 67 68 69 70 73
# a_vec < 0.1 & a_vec >0.05
# 1 32 50 53 54

# Variablen < 0.05 Signifikanz
colnames(final_data_relevant_1[, c(46, 53, 65, 66, 67, 68, 69, 70, 71, 74)])
# "POS_NEG_O_CF_J"         "J_var3_VJ"              "entity_specific_tag"   
# [4] "entity_specific_tag_VJ" "issue_X"                "issue_X1"              
# [7] "issue_X2"               "issue_X_real"           "issue_sum"             
# [10] "MJ_var_3"        

# Variablen 0.05 < x < 0.1 Signifikanz
colnames(final_data_relevant_1[, c(2, 33, 51, 54, 55)])

# "DSRI"          "SOFT_ASSET_VJ" "J_var3_J"      "J_var2_4"      "J_var3_4"


####################################
# Lasso-Regression:
####################################

x<-as.matrix(final_data_relevant_1_v1[,c(2:66)])
y<-final_data_relevant_1_v1$FRAUD_1_NONFRAUD_0
#class(final_data_relevant_2$FRAUD_1_NONFRAUD_0)

cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

plot(cv.lasso)
cv.lasso$lambda.min
coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, 0.15)

# entity_specific_tag     0.8432203
# entity_specific_tag_VJ  2.6196128
# issue_X_real            0.1780908
# issue_sum               .        
# MJ_var_J                .        
# MJ_var_VJ               .        
# MJ_var_3                0.1388796




final_data_relevant_1_v2<-final_data_relevant_1_v1[, names(final_data_relevant_1_v1) %in% 
                                                  c("FRAUD_1_NONFRAUD_0",
                                                    "entity_specific_tag", "entity_specific_tag_VJ",
                                                    "issue_sum", "POS_NEG_O_CF_J", "POS_NEG_O_CF_VJ", 
                                                    "POS_NEG_O_CF_4", "PMR_J", "PMR_VJ", "PMR_4",
                                                    "ACCR_CF_J", "ACCR_CF_VJ", "ACCR_CF_4", 
                                                    "DEF_TAX_EXP_J", "DEF_TAX_EXP_VJ",
                                                    "J_var3_J", "J_var3_VJ", "J_var3_4",
                                                    "F_Score_Result", 
                                                    "MJ_var2_J", "MJ_var2_VJ", "MJ_var2_4",
                                                    "DSRI", "DSRI_1",
                                                    "ABW_CF_ABS_MJ", 
                                                    "ZSCORE_4_J", "ZSCORE_4_VJ"
              
                                                  )] 


dim(final_data_relevant_1_v2) # 87, 27 Variablen (inkl Y)
colnames(final_data_relevant_1_v2)

final_data_relevant_1_v3<-final_data_relevant_1_v2[, names(final_data_relevant_1_v2) %in% 
                                                     c("FRAUD_1_NONFRAUD_0",
                                                       "DSRI", "PMR_4", "DEF_TAX_EXP_VJ", "ZSCORE_4_J", "ZSCORE_4_VJ",
                                                       "POS_NEG_O_CF_J", "POS_NEG_O_CF_VJ", "J_var3_VJ", "J_var3_4",
                                                       "MJ_var2_J", "MJ_var2_VJ", "MJ_var2_4", "entity_specific_tag",
                                                       "issue_sum"
                                                     )] 

final_data_relevant_1_v3$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v3$FRAUD_1_NONFRAUD_0)

model2 <- glm(FRAUD_1_NONFRAUD_0 ~., 
              data = final_data_relevant_1_v3, family = binomial)

model2 <- glm(FRAUD_1_NONFRAUD_0 ~., 
              data = final_data_relevant_1_v3, family = binomial) %>%
  stepAIC(trace = FALSE, direction="backward")

summary(model2)

# Estimate Std. Error z value Pr(>|z|)   
# (Intercept)           -4.64624    2.71995  -1.708  0.08760 . 
# DSRI                   2.88224    1.31534   2.191  0.02843 * 
#   PMR_4                  2.86974    1.39422   2.058  0.03956 * 
#   DEF_TAX_EXP_VJ      -174.07479   68.55569  -2.539  0.01111 * 
#   ZSCORE_4_J             0.21875    0.08446   2.590  0.00960 **
#   ZSCORE_4_VJ           -0.16860    0.07984  -2.112  0.03470 * 
#   POS_NEG_O_CF_J       -14.43351    5.43037  -2.658  0.00786 **
#   POS_NEG_O_CF_VJ       13.72939    5.18913   2.646  0.00815 **
#   J_var3_VJ            -49.86389   22.11054  -2.255  0.02412 * 
#   J_var3_4              44.24590   23.34312   1.895  0.05803 . 
# MJ_var2_J            -15.20934    6.20573  -2.451  0.01425 * 
#   MJ_var2_VJ            -4.56109    3.48232  -1.310  0.19027   
# MJ_var2_4              6.97816    3.48405   2.003  0.04519 * 
#   entity_specific_tag   15.12025    6.47635   2.335  0.01956 * 
#   issue_sum              0.61985    0.25995   2.384  0.01710 * 
# # AIC: 76.554

##########

final_data_relevant_1_v4<-final_data_relevant_1_v3[, names(final_data_relevant_1_v3) %in% 
                                                     c("FRAUD_1_NONFRAUD_0",
                                                       "DSRI", "PMR_4", "DEF_TAX_EXP_VJ", "ZSCORE_4_J", "ZSCORE_4_VJ",
                                                       "POS_NEG_O_CF_J", "POS_NEG_O_CF_VJ", "J_var3_VJ", "J_var3_4",
                                                       "MJ_var2_J", "MJ_var2_4", "entity_specific_tag",
                                                       "issue_sum"
                                                     )] 

model2 <- glm(FRAUD_1_NONFRAUD_0 ~., 
              data = final_data_relevant_1_v4, family = binomial) %>%
  stepAIC(trace = FALSE, direction="both")

summary(model2)


final_data_relevant_1_v4<-final_data_relevant_1_v3[, names(final_data_relevant_1_v3) %in% 
                                                     c("FRAUD_1_NONFRAUD_0",
                                                       "DSRI", "PMR_4", "DEF_TAX_EXP_VJ", "ZSCORE_4_J", "ZSCORE_4_VJ",
                                                       "POS_NEG_O_CF_J", "POS_NEG_O_CF_VJ", "J_var3_VJ",
                                                       "MJ_var2_J", "MJ_var2_4", "entity_specific_tag",
                                                       "issue_sum"
                                                     )] 

model2 <- glm(FRAUD_1_NONFRAUD_0 ~., 
              data = final_data_relevant_1_v4, family = binomial)


model2 <- glm(FRAUD_1_NONFRAUD_0 ~., 
              data = final_data_relevant_1_v4, family = binomial) %>%
  stepAIC(trace = FALSE, direction="both")



#####################################################################

# Prüfen auf Klassifikationfähigkeiten

model2 <- glm(FRAUD_1_NONFRAUD_0 ~., 
              data = final_data_relevant_1_v4, family = binomial) %>%
  stepAIC(trace = FALSE, direction="both")

summary(model2)






final_data_relevant_1_v4 <- final_data_relevant_1_v4[,c("FRAUD_1_NONFRAUD_0", "DSRI", "PMR_4", "DEF_TAX_EXP_VJ", 
                                                        "ZSCORE_4_J", "ZSCORE_4_VJ", "POS_NEG_O_CF_J", "POS_NEG_O_CF_VJ",
                                                        "J_var3_VJ", "MJ_var2_J", "MJ_var2_4", "entity_specific_tag", "issue_sum")]

colnames(final_data_relevant_1_v4)
final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0)
final_data_relevant_1_v4$POS_NEG_O_CF_J <- as.factor(final_data_relevant_1_v4$POS_NEG_O_CF_J)
final_data_relevant_1_v4$POS_NEG_O_CF_VJ <- as.factor(final_data_relevant_1_v4$POS_NEG_O_CF_VJ)
final_data_relevant_1_v4$issue_sum <- as.factor(final_data_relevant_1_v4$issue_sum)


str(final_data_relevant_1_v4)


final_data_relevant_1_v4 <- final_data_relevant_1_v4[,c("FRAUD_1_NONFRAUD_0", "DSRI", "PMR_4", "DEF_TAX_EXP_VJ", 
                                                        "ZSCORE_4_J", "ZSCORE_4_VJ", "POS_NEG_O_CF_J", "POS_NEG_O_CF_VJ",
                                                        "J_var3_VJ", "MJ_var2_J", "MJ_var2_4", "entity_specific_tag", "issue_sum")]

