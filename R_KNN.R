


### Künstliche Neurale Netze:

# deepnet + over- undersampling

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

library(randomForest)


library(FSelector)
library(imbalance)
library(deepnet)
library(dplyr)
library(caret)
library(imbalance)
library(ROSE)



# Deepnet - hier gehörst du hin

final_data_relevant

?nn.train
# deepnet - Package

#############################
# Ausgehend von nachfolgenden Code von oben - Cross-Validation durchführen
# k-Fold - es gibt lt. div. Quellen keine Built-In Funktion
# https://www.r-bloggers.com/2015/09/fitting-a-neural-network-in-r-neuralnet-package/

# nur numerische Spalten
num_df<-final_data_relevant_1_v2[,c(2:10,14:22, 23,24, 26,27)]

maxs <- apply(num_df, 2, max) 
mins <- apply(num_df, 2, min)
scaled <- as.data.frame(scale(num_df, center = mins, scale = maxs - mins))

# add binary columns
# final_data_relevant_1_v2[,c(1, 11,12, 13 ,25)]

scaled$POS_NEG_O_CF_J <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_J)
scaled$POS_NEG_O_CF_VJ <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_VJ)
scaled$POS_NEG_O_CF_4 <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_4)
scaled$issue_sum <- as.factor(final_data_relevant_1_v2$issue_sum)
scaled$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0)

dim(scaled) # 87 27
summary(scaled)



training.samples <- scaled$FRAUD_1_NONFRAUD_0 %>% 
  createDataPartition(p = 0.7, list = FALSE)
# Zuweisung der Indexwerte zu Train und Test
train.data  <- scaled[training.samples, ]
test.data <- scaled[-training.samples, ]



# Funktion mit der die Zielvariable als Vektor in eine Matrix umwandle
# dies ist erforderlich für das Package deepnet
class.ind <- function(cl)
{
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)) )
  x[(1:n) + n*(unclass(cl)-1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}

train_label <- class.ind(train.data$FRAUD_1_NONFRAUD_0)
test_label = class.ind(test.data$FRAUD_1_NONFRAUD_0)






set.seed(123)
# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()


# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()


# install_keras(tensorflow="gpu")

for (i in 1:100) {
  
  training.samples <- scaled$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- scaled[training.samples, ]
  test.data <- scaled[-training.samples, ]
  
  train_label <- class.ind(train.data$FRAUD_1_NONFRAUD_0)
  test_label = class.ind(test.data$FRAUD_1_NONFRAUD_0)
  
  # Over/undersampling Methode: 
  # both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5)$data
  
  # Transformation in Matrizen - deepnet kann nur Matrizen lesen - keine DF
  # bei Anwendung von Over/undersampling Methode muss für train.data mit both_train ausgetauscht werden
  
  train_matrix <- as.numeric(as.matrix(train.data[,1:26])) 
  train_matrix <- matrix(as.numeric(train_matrix),ncol=26)
  test_matrix <- as.numeric(as.matrix(test.data[,c(1:26)]))
  test_matrix <- matrix(as.numeric(test_matrix),ncol=26)
  
  
  # Train NN model
  N <- nn.train(train_matrix, train_label, 
                hidden = c(10),          
                activationfun = "sigm",        
                batchsize = 10,  # number of samples in each batch - before updated
                learningrate = 1, 
                output="softmax",
                numepochs = 3)
  
  
  
  # output="sigm")                # activation function for output layer: sigmoid
  #output="softmax")             # activation function for output layer: softmax
  #batchsize=5,
  #numepochs=10,
  #learningrate=0.1)
  
  # zwei Spalten als ERgebnisse:
  
  # nn.predict(N, train_matrix)
  # 
  # success_n = sum(max.col(pred_n_tr) == max.col(cil_tr))/length(labels_tr)
  
  #########################################################
  # Classify train data
  pred_n_tr = nn.predict(N, train_matrix)
  
  senstr<- sum(max.col(pred_n_tr)[1:20] == max.col(train_label)[1:20])/20
  speztr<- sum(max.col(pred_n_tr)[21:62] == max.col(train_label)[21:62])/42
  res_tr<- sum(max.col(pred_n_tr) == max.col(train_label))/62
  
  #speichern in Vektoren
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
  # yhat = matrix(0,length(pred_n_tr),1)
  # 
  # # vorhergesagte Wert > Mittelwert = 1, sonst 0 
  # yhat[which(pred_n_tr > mean(pred_n_tr))] = 1
  # yhat[which(pred_n_tr <= mean(pred_n_tr))] = 0
  # 
  # # Ergebnisse in confusion matrix speichern
  # cm = table(train_label,yhat)
  # 
  # # Sensitivität TRAIN
  # senstr<-cm[4]/(cm[2] + cm[4])
  # 
  # # Spezifität TRAIN
  # speztr<-cm[1]/(cm[1] + cm[3])
  # 
  # # Gesamttrefferquote TRAIN
  # res_tr<-(cm[1]+cm[4])/(cm[1] + cm[2] + cm[3] + cm[4])
  
  
  
  
  
  ################################################
  
  #########################################################
  # Classify test data
  pred_n_te = nn.predict(N, test_matrix)
  
  sens_t<- sum(max.col(pred_n_te)[1:8] == max.col(test_label)[1:8])/8
  spez_t<- sum(max.col(pred_n_te)[9:25] == max.col(test_label)[9:25])/17
  res_te<- sum(max.col(pred_n_te) == max.col(test_label))/25
  
  # ytest = matrix(0,length(pred_n_te),1)
  # 
  # ytest[which(pred_n_te > mean(pred_n_te))] = 1
  # ytest[which(pred_n_te <= mean(pred_n_te))] = 0
  # 
  # cm_te = table(test_label,ytest)
  # 
  # # Sensitivität TRAIN
  # sens_t<-cm_te[4]/(cm_te[2] + cm_te[4])
  # 
  # # Spezifität TRAIN
  # spez_t<-cm_te[1]/(cm_te[1] + cm_te[3])
  # 
  # # Gesamttrefferquote TRAIN
  # res_te<-(cm_te[1]+cm_te[4])/(cm_te[1] + cm_te[2] + cm_te[3] + cm_te[4])
  
  sens_test<-c(sens_test, sens_t)
  spez_test<-c(spez_test, spez_t)
  test_res<-c(test_res, res_te)
  
  
  
}



mean(sens_train)*100
mean(spez_train)*100
mean(train_res)*100

mean(sens_test)*100
mean(spez_test)*100
mean(test_res)*100

################################################################################
#################################################################################
####################################################################################



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









final_data_relevant_1_v4<-final_data_relevant_1_v2[, names(final_data_relevant_1_v2) %in% 
                                                     c("FRAUD_1_NONFRAUD_0",
                                                       "DSRI", "PMR_4", "DEF_TAX_EXP_VJ", "ZSCORE_4_J", "ZSCORE_4_VJ",
                                                       "POS_NEG_O_CF_J", "POS_NEG_O_CF_VJ", "J_var3_VJ", "J_var3_4",
                                                       "MJ_var2_J", "MJ_var2_4", "entity_specific_tag",
                                                       "issue_sum"
                                                     )] 

# Datensatz v4 --> Datensatz 2
num_df<-final_data_relevant_1_v4[,c(2:6,9:12)]

# Datensatz v2 --> Datensatz 1
# nur numerische Spalten
num_df<-final_data_relevant_1_v2[,c(2:10,14:22, 23,24, 26,27)]



# Max- und Min-Werte von allen Variablen ermitteln
maxs <- apply(num_df, 2, max) 
mins <- apply(num_df, 2, min)

# Skalierung der Werte
scaled <- as.data.frame(scale(num_df, center = mins, scale = maxs - mins))

# Wiedereinfügen der nicht numerischen Werte (nominelle bzw. ordinalen Werte)
scaled$POS_NEG_O_CF_J <- as.factor(final_data_relevant_1_v4$POS_NEG_O_CF_J)
scaled$POS_NEG_O_CF_VJ <- as.factor(final_data_relevant_1_v4$POS_NEG_O_CF_VJ)
scaled$issue_sum <- as.factor(final_data_relevant_1_v4$issue_sum)
scaled$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0)


# Random-Wert initalisieren
set.seed(123)



# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()


# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()



for (i in 1:1000) {
  
  training.samples <- scaled$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- scaled[training.samples, ]
  test.data <- scaled[-training.samples, ]
  
  # both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5)$data
  
  # K-Nearest Neighbour - zufällig gewählt in jedem Loop
  # stellt sicher, dass SMOTE-Datensatz immer ein wenig anders erzeugt wird
  k_noisy = round(runif(1, min=2, max=10),0)
  k_major = round(runif(1, min=2, max=5),0)
  k_mino = round(runif(1, min=6, max=10),0)
  
  
  # 40 zusätzliche synthetische Trainingsdaten erzeugt
  smote_train<-mwmote(train.data, numInstances = 150, 
                      kNoisy = k_noisy, 
                      kMajority = k_major,
                      kMinority = k_mino,
                      classAttr = "FRAUD_1_NONFRAUD_0")
  
  # Factor-Daten teilweise mit Werten von 0.24... statt 0 oder 1
  # daher auf/abrunden was näher ist und zurückwandeln in Faktorwert
  smote_train$POS_NEG_O_CF_J<- as.factor(round(as.numeric(as.character(smote_train$POS_NEG_O_CF_J)),0))
  smote_train$POS_NEG_O_CF_VJ<- as.factor(round(as.numeric(as.character(smote_train$POS_NEG_O_CF_VJ)),0))
  #smote_train$POS_NEG_O_CF_4<- as.factor(round(as.numeric(as.character(smote_train$POS_NEG_O_CF_4)),0))
  smote_train$issue_sum <- as.factor(round(as.numeric(as.character(smote_train$issue_sum)),0))
  
  # Zusammensetzen der Trainingsdaten (echt) + synthetische Daten 
  both_train<-rbind(train.data, smote_train)
  
  
  
  first_last_v<-tail(which(as.numeric(train.data$FRAUD_1_NONFRAUD_0) > 1),1)
  length_fraud<-length(which(as.numeric(train.data$FRAUD_1_NONFRAUD_0) > 1))
  length_non_fraud<-length(which(as.numeric(train.data$FRAUD_1_NONFRAUD_0) == 1))
  
  train_label <- class.ind(both_train$FRAUD_1_NONFRAUD_0)
  test_label = class.ind(test.data$FRAUD_1_NONFRAUD_0)
  real_train_label <- class.ind(train.data$FRAUD_1_NONFRAUD_0)
  
  
  
  # Transformation in Matrizen - deepnet kann nur Matrizen lesen - keine DF
  train_matrix <- as.numeric(as.matrix(both_train[,1:12]))
  train_matrix <- matrix(as.numeric(train_matrix),ncol=12)
  test_matrix <- as.numeric(as.matrix(test.data[,c(1:12)]))
  test_matrix <- matrix(as.numeric(test_matrix),ncol=12)
  
  real_train_matrix <- as.numeric(as.matrix(train.data[,1:12]))
  real_train_matrix <- matrix(as.numeric(real_train_matrix),ncol=12)
  
  # Train NN model
  N <- nn.train(train_matrix, train_label, 
                hidden = c(10),          
                activationfun = "tanh",        
                batchsize = 8,  # number of samples in each batch - before updated
                learningrate = 2,
                learningrate_scale = 0.95, 
                output="softmax",
                numepochs = 500,
                momentum = 0.75,
                #visible_dropout = 0,
                hidden_dropout = 0.5)
  
  
  
  # output="sigm")                # activation function for output layer: sigmoid
  #output="softmax")             # activation function for output layer: softmax
  #batchsize=5,
  #numepochs=10,
  #learningrate=0.1)
  
  # zwei Spalten als ERgebnisse:
  
  # nn.predict(N, train_matrix)
  # 
  # success_n = sum(max.col(pred_n_tr) == max.col(cil_tr))/length(labels_tr)
  
  #########################################################
  # Classify train data
  pred_n_tr = nn.predict(N, real_train_matrix)
  
  senstr<- sum(max.col(pred_n_tr)[1:first_last_v] == max.col(real_train_label)[1:first_last_v])/length_fraud
  speztr<- sum(max.col(pred_n_tr)[(first_last_v+1):62] == max.col(real_train_label)[(first_last_v+1):62])/length_non_fraud
  res_tr<- sum(max.col(pred_n_tr) == max.col(real_train_label))/(length_non_fraud + length_fraud)
  
  # speztr<- sum(max.col(pred_n_tr)[1:first_last_v] == max.col(train_label)[1:first_last_v])/first_last_v
  # senstr<- sum(max.col(pred_n_tr)[(first_last_v+1):62] == max.col(train_label)[(first_last_v+1):62])/length_fraud
  # res_tr<- sum(max.col(pred_n_tr) == max.col(train_label))/62
  
  #speichern in Vektoren
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
  
  ################################################
  
  #########################################################
  # Classify test data
  pred_n_te = nn.predict(N, test_matrix)
  
  sens_t<- sum(max.col(pred_n_te)[1:8] == max.col(test_label)[1:8])/8
  spez_t<- sum(max.col(pred_n_te)[9:25] == max.col(test_label)[9:25])/17
  res_te<- sum(max.col(pred_n_te) == max.col(test_label))/25
  
  
  sens_test<-c(sens_test, sens_t)
  spez_test<-c(spez_test, spez_t)
  test_res<-c(test_res, res_te)
  
  
  
}



mean(sens_train)*100
mean(spez_train)*100
mean(train_res)*100

mean(sens_test)*100
mean(spez_test)*100
mean(test_res)*100


round(pred_n_tr,2)







#############################################
# Test Datensatz 2 mit 27 Variablen


#############################
# Ausgehend von nachfolgenden Code von oben - Cross-Validation durchführen
# k-Fold - es gibt lt. div. Quellen keine Built-In Funktion
# https://www.r-bloggers.com/2015/09/fitting-a-neural-network-in-r-neuralnet-package/

dim(final_data_relevant_1_v2)
# nur numerische Spalten
num_df<-final_data_relevant_1_v2[,c(2:10,14:22, 23,24, 26,27)]

maxs <- apply(num_df, 2, max) 
mins <- apply(num_df, 2, min)
scaled <- as.data.frame(scale(num_df, center = mins, scale = maxs - mins))

# add binary columns
# final_data_relevant_1_v2[,c(1, 11,12, 13 ,25)]

scaled$POS_NEG_O_CF_J <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_J)
scaled$POS_NEG_O_CF_VJ <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_VJ)
scaled$POS_NEG_O_CF_4 <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_4)
scaled$issue_sum <- as.factor(final_data_relevant_1_v2$issue_sum)
scaled$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0)

dim(scaled) # 87 27
summary(scaled)


smote_train<-mwmote(train.data, numInstances = 40, 
                    classAttr = "FRAUD_1_NONFRAUD_0")

# Factor-Daten teilweise mit Werten von 0.24... statt 0 oder 1
# daher auf/abrunden was näher ist und zurückwandeln in Faktorwert
smote_train$POS_NEG_O_CF_J<- as.factor(round(as.numeric(as.character(smote_train$POS_NEG_O_CF_J)),0))
smote_train$POS_NEG_O_CF_VJ<- as.factor(round(as.numeric(as.character(smote_train$POS_NEG_O_CF_VJ)),0))
smote_train$POS_NEG_O_CF_4<- as.factor(round(as.numeric(as.character(smote_train$POS_NEG_O_CF_4)),0))
smote_train$issue_sum <- as.factor(round(as.numeric(as.character(smote_train$issue_sum)),0))

str(smote_train)
str(train.data)

# Daten zusammensetzen - Train und synthetische Daten
train_data_comb<-rbind(train.data, smote_train)


# duhier-affe




# training.samples <- scaled$FRAUD_1_NONFRAUD_0 %>% 
#   createDataPartition(p = 0.7, list = FALSE)
# # Zuweisung der Indexwerte zu Train und Test
# train.data  <- scaled[training.samples, ]
# test.data <- scaled[-training.samples, ]

class.ind <- function(cl)
{
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)) )
  x[(1:n) + n*(unclass(cl)-1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}


set.seed(123)
# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()


# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()

#dim(train.data)

# mwmote

# ?nn.train
# ?mwmote

# install_keras(tensorflow="gpu")

for (i in 1:100) {
  
  training.samples <- scaled$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- scaled[training.samples, ]
  test.data <- scaled[-training.samples, ]
  
  # both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5)$data
  
  # K-Nearest Neighbour - zufällig gewählt in jedem Loop
  # stellt sicher, dass SMOTE-Datensatz immer ein wenig anders erzeugt wird
  k_noisy = round(runif(1, min=2, max=10),0)
  k_major = round(runif(1, min=2, max=5),0)
  k_mino = round(runif(1, min=6, max=10),0)
  
  
  # 40 zusätzliche synthetische Trainingsdaten erzeugt
  smote_train<-mwmote(train.data, numInstances = 80, 
                      kNoisy = k_noisy, 
                      kMajority = k_major,
                      kMinority = k_mino,
                      classAttr = "FRAUD_1_NONFRAUD_0")
  
  # Factor-Daten teilweise mit Werten von 0.24... statt 0 oder 1
  # daher auf/abrunden was näher ist und zurückwandeln in Faktorwert
  smote_train$POS_NEG_O_CF_J<- as.factor(round(as.numeric(as.character(smote_train$POS_NEG_O_CF_J)),0))
  smote_train$POS_NEG_O_CF_VJ<- as.factor(round(as.numeric(as.character(smote_train$POS_NEG_O_CF_VJ)),0))
  smote_train$POS_NEG_O_CF_4<- as.factor(round(as.numeric(as.character(smote_train$POS_NEG_O_CF_4)),0))
  smote_train$issue_sum <- as.factor(round(as.numeric(as.character(smote_train$issue_sum)),0))
  
  # Zusammensetzen der Trainingsdaten (echt) + synthetische Daten 
  both_train<-rbind(train.data, smote_train)
  
  
  
  first_last_v<-tail(which(as.numeric(train.data$FRAUD_1_NONFRAUD_0) > 1),1)
  length_fraud<-length(which(as.numeric(train.data$FRAUD_1_NONFRAUD_0) > 1))
  length_non_fraud<-length(which(as.numeric(train.data$FRAUD_1_NONFRAUD_0) == 1))
  
  train_label <- class.ind(both_train$FRAUD_1_NONFRAUD_0)
  test_label = class.ind(test.data$FRAUD_1_NONFRAUD_0)
  real_train_label <- class.ind(train.data$FRAUD_1_NONFRAUD_0)
  
  
  
  # Transformation in Matrizen - deepnet kann nur Matrizen lesen - keine DF
  train_matrix <- as.numeric(as.matrix(both_train[,1:26]))
  train_matrix <- matrix(as.numeric(train_matrix),ncol=26)
  test_matrix <- as.numeric(as.matrix(test.data[,c(1:26)]))
  test_matrix <- matrix(as.numeric(test_matrix),ncol=26)
  
  real_train_matrix <- as.numeric(as.matrix(train.data[,1:26]))
  real_train_matrix <- matrix(as.numeric(real_train_matrix),ncol=26)
  
  # Train NN model
  N <- nn.train(train_matrix, train_label, 
                hidden = c(10),          
                activationfun = "tanh",        
                batchsize = 8,  # number of samples in each batch - before updated
                learningrate = 2,
                learningrate_scale = 0.95, 
                output="softmax",
                numepochs = 500,
                momentum = 0.1,
                visible_dropout = 0,
                hidden_dropout = 0.5)
  
  
  
  # output="sigm")                # activation function for output layer: sigmoid
  #output="softmax")             # activation function for output layer: softmax
  #batchsize=5,
  #numepochs=10,
  #learningrate=0.1)
  
  # zwei Spalten als ERgebnisse:
  
  # nn.predict(N, train_matrix)
  # 
  # success_n = sum(max.col(pred_n_tr) == max.col(cil_tr))/length(labels_tr)
  
  #########################################################
  # Classify train data
  pred_n_tr = nn.predict(N, real_train_matrix)
  
  senstr<- sum(max.col(pred_n_tr)[1:first_last_v] == max.col(real_train_label)[1:first_last_v])/length_fraud
  speztr<- sum(max.col(pred_n_tr)[(first_last_v+1):62] == max.col(real_train_label)[(first_last_v+1):62])/length_non_fraud
  res_tr<- sum(max.col(pred_n_tr) == max.col(real_train_label))/(length_non_fraud + length_fraud)
  
  # speztr<- sum(max.col(pred_n_tr)[1:first_last_v] == max.col(train_label)[1:first_last_v])/first_last_v
  # senstr<- sum(max.col(pred_n_tr)[(first_last_v+1):62] == max.col(train_label)[(first_last_v+1):62])/length_fraud
  # res_tr<- sum(max.col(pred_n_tr) == max.col(train_label))/62
  
  #speichern in Vektoren
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
  
  ################################################
  
  #########################################################
  # Classify test data
  pred_n_te = nn.predict(N, test_matrix)
  
  sens_t<- sum(max.col(pred_n_te)[1:8] == max.col(test_label)[1:8])/8
  spez_t<- sum(max.col(pred_n_te)[9:25] == max.col(test_label)[9:25])/17
  res_te<- sum(max.col(pred_n_te) == max.col(test_label))/25
  
  
  sens_test<-c(sens_test, sens_t)
  spez_test<-c(spez_test, spez_t)
  test_res<-c(test_res, res_te)
  
  
  
}



mean(sens_train)*100
mean(spez_train)*100
mean(train_res)*100

mean(sens_test)*100
mean(spez_test)*100
mean(test_res)*100


round(pred_n_tr,2)



######################################################################################
#######################################################################################
######################################################################################
#######################################################################################
######################################################################################
#######################################################################################

# h20




###################################################################################
################################################################################
#################################################################################


# Neural Network with H20-Package

library(h2o)

?h2o

# h2o initalisieren:
h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
# R is connected to the H2O cluster: 
#   H2O cluster uptime:         3 seconds 631 milliseconds 
# H2O cluster timezone:       Europe/Berlin 
# H2O data parsing timezone:  UTC 
# H2O cluster version:        3.36.1.2 
# H2O cluster version age:    3 months  
# H2O cluster name:           H2O_started_from_R_Startklar_mmf682 
# H2O cluster total nodes:    1 
# H2O cluster total memory:   3.40 GB 
# H2O cluster total cores:    6 
# H2O cluster allowed cores:  6 
# H2O cluster healthy:        TRUE 
# H2O Connection ip:          localhost 
# H2O Connection port:        54321 
# H2O Connection proxy:       NA 
# H2O Internal Security:      FALSE 
# R Version:                  R version 4.1.1 (2021-08-10) 

# convert input-DF into h2o-Frame
scaled_h2o<-as.h2o(scaled)

h2o.describe(scaled_h2o)

################################################
###########################################################################
########################################################################
# nur numerische Spalten

num_df<-final_data_relevant_1_v2[,c(2:10,14:22, 23,24, 26,27)]

num_df$POS_NEG_O_CF_J <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_J)
num_df$POS_NEG_O_CF_VJ <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_VJ)
num_df$POS_NEG_O_CF_4 <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_4)
num_df$issue_sum <- as.factor(final_data_relevant_1_v2$issue_sum)
num_df$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0)
# weights_column einfügen - 
num_df$weights <- c(rep(5, each = 1, len = 28), rep(1, each = 1, len = 59))


y <- "FRAUD_1_NONFRAUD_0"
predictors <- setdiff(names(num_df), y)
dataset_2_h2o<-as.h2o(num_df)



set.seed(123)
# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()


# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()

for (i in 1:1000) {
  
  training.samples <- final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- dataset_2_h2o[training.samples, ]
  test.data <- dataset_2_h2o[-training.samples, ]
  
  # both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5)$data
  
  # first_last_v<-which(as.numeric(train.data$FRAUD_1_NONFRAUD_0) > 1)[1]
  # length_fraud<-length(which(as.numeric(train.data$FRAUD_1_NONFRAUD_0) > 1))
  
  
  # Train NN model
  model <- h2o.deeplearning(x = 1:26,
                            y = 27,
                            training_frame = train.data,
                            nfolds = 0,
                            standardize = TRUE,
                            activation = "Rectifier",
                            weights_column = "weights",
                            hidden = c(10),
                            epochs = 100,
                            rate = 0.01,
                            momentum_stable = 0.9)
  
  
  
  
  
  #########################################################
  # Classify train data
  
  
  predict_train<-h2o.predict(object = model, 
                             newdata = train.data)
  
  predict_train<-as.data.frame(predict_train)
  p_train<-as.numeric(predict_train$predict)
  
  
  train_v<-as.data.frame(train.data$FRAUD_1_NONFRAUD_0)
  train_v<-as.numeric(train_v$FRAUD_1_NONFRAUD_0)
  # https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/class_sampling_factors.html
  
  senstr<- sum((p_train)[1:20] == (train_v)[1:20])/20
  speztr<- sum((p_train)[21:62] == (train_v)[21:62])/42
  res_tr<- sum((p_train) == (train_v))/62
  
  #speichern in Vektoren
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
  
  ################################################
  
  #########################################################
  # Classify test data
  
  predict_test<-h2o.predict(object = model, 
                            newdata = test.data)
  
  predict_test<-as.data.frame(predict_test)
  p_test<-as.numeric(predict_test$predict)
  
  test_v<-as.data.frame(test.data$FRAUD_1_NONFRAUD_0)
  test_v<-as.numeric(test_v$FRAUD_1_NONFRAUD_0)
  
  senst<- sum((p_test)[1:8] == (test_v)[1:8])/8
  spezt<- sum((p_test)[9:25] == (test_v)[9:25])/17
  res_te<- sum((p_test) == (test_v))/25
  
  
  sens_test<-c(sens_test, senst)
  spez_test<-c(spez_test, spezt)
  test_res<-c(test_res, res_te)
  
  
  
}

length(sens_train)

mean(sens_train)*100
mean(spez_train)*100
mean(train_res)*100

mean(sens_test)*100
mean(spez_test)*100
mean(test_res)*100


h2o.shutdown(prompt = TRUE)