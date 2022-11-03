

#####################
# Support Vector Machines  SVM

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

library(ROSE)
library("e1071")

# Datensatz 1 --> final_data_relevant_1_v2

# ohne over -under sampling

# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()


# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()

#dim(train.data)
# train.data$FRAUD_1_NONFRAUD_0
# train.data$FRAUD_1_NONFRAUD_0[1:20]
# train.data$FRAUD_1_NONFRAUD_0[21:62]

for (i in 1:1000) {
  
  training.samples <- final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- final_data_relevant_1_v2[training.samples, ]
  test.data <- final_data_relevant_1_v2[-training.samples, ]
  
  both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5)$data
  
  model = svm(formula = FRAUD_1_NONFRAUD_0 ~ .,
              data = both_train,
              type = 'C-classification',
              kernel = 'polynomial',
              cost = 0.1, degree=3, gamma=0.1)
  
  # , degree=3
  predict_both <- predict(model, test.data, seed=1234)
  
  # TEST-DATEN - Sensitvität und Spezifität
  sens_t<-mean(predict_both[1:8] == test.data$FRAUD_1_NONFRAUD_0[1:8])
  spez_t<-mean(predict_both[9:25] == test.data$FRAUD_1_NONFRAUD_0[9:25])
  res_te<-mean(predict_both == test.data$FRAUD_1_NONFRAUD_0)
  
  sens_test<-c(sens_test, sens_t)
  spez_test<-c(spez_test, spez_t)
  test_res<-c(test_res, res_te)
  
  # TRAININGS-DATEN - Sensitvität und Spezifität
  speztr<-mean(train.data$FRAUD_1_NONFRAUD_0[1:20] == model$fitted[1:20])
  senstr<-mean(train.data$FRAUD_1_NONFRAUD_0[21:62] == model$fitted[21:62])
  res_tr<-mean(train.data$FRAUD_1_NONFRAUD_0 == model$fitted)
  
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
}

#length(predict_both)
#rf_both$forest
#table(both_train$FRAUD_1_NONFRAUD_0)
#dim(both_train)

#distinct(both_train[c(42:82),])

mean(sens_train)*100
mean(spez_train)*100
mean(train_res)*100

mean(sens_test)*100
mean(spez_test)*100
mean(test_res)*100




##########################################################################
dim(both_train)
colnames(final_data_relevant_1_v2)

final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0)
final_data_relevant_1_v4$POS_NEG_O_CF_J <- as.factor(final_data_relevant_1_v4$POS_NEG_O_CF_J)
final_data_relevant_1_v4$POS_NEG_O_CF_VJ <- as.factor(final_data_relevant_1_v4$POS_NEG_O_CF_VJ)
final_data_relevant_1_v4$issue_sum <- as.factor(final_data_relevant_1_v4$issue_sum)

dim(final_data_relevant_1_v2)

# http://www.di.fc.ul.pt/~jpn/r/svm/svm.html
weightss <- table(final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0)  # the weight vector must be named with the classes names
weightss[2] <- 5 # a class -1 mismatch has a terrible cost, 1e10 --> 1 with 10 Zeros
weightss[1] <- 1    # a class +1 mismatch not so much...
weightss

#set.seed(123)
# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()


# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()

#model
# colnames(final_data_relevant_1_v4)
# str(final_data_relevant_1_v4)
# ?svm
# dim(final_data_relevant_1_v2)

for (i in 1:1000) {
  
  training.samples <- final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- final_data_relevant_1_v2[training.samples, ]
  test.data <- final_data_relevant_1_v2[-training.samples, ]
  
  both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5)$data
  
  model = svm(formula = FRAUD_1_NONFRAUD_0 ~ .,
              data = both_train,
              type = 'C-classification',
              kernel = 'polynomial',
              class.weigths=weightss,
              degree= 3, cost=0.6, gamma=0.2
              
  )
  # class.weigths=weightss,
  # cost=0.3,
  # gamma=0.1
  # cost = 0.08,
  # gamma=0.1,
  # class.weigths=weightss,
  # coef=1
  # , class.weight=weightss, gamma=0.1
  # , class.weights=weightss, coef=1.1, scale=TRUE, epsilon = 100 , tolerance= 1
  # , gamma=0.1, 
  # kernel = 'polynomial', degree=3, , gamma=0.5 
  predict_both <- predict(model, test.data, seed=1234)
  
  # TEST-DATEN - Sensitvität und Spezifität
  sens_t<-mean(predict_both[1:8] == test.data$FRAUD_1_NONFRAUD_0[1:8])
  spez_t<-mean(predict_both[9:25] == test.data$FRAUD_1_NONFRAUD_0[9:25])
  res_te<-mean(predict_both == test.data$FRAUD_1_NONFRAUD_0)
  
  sens_test<-c(sens_test, sens_t)
  spez_test<-c(spez_test, spez_t)
  test_res<-c(test_res, res_te)
  
  # TRAININGS-DATEN - Sensitvität und Spezifität
  speztr<-mean(both_train$FRAUD_1_NONFRAUD_0[1:31] == model$fitted[1:31])
  senstr<-mean(both_train$FRAUD_1_NONFRAUD_0[32:62] == model$fitted[32:62])
  res_tr<-mean(both_train$FRAUD_1_NONFRAUD_0 == model$fitted)
  
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
}

#length(predict_both)
#rf_both$forest
#table(both_train$FRAUD_1_NONFRAUD_0)
#dim(both_train)

#distinct(both_train[c(42:82),])

mean(sens_train)*100
mean(spez_train)*100
mean(train_res)*100

mean(sens_test)*100
mean(spez_test)*100
mean(test_res)*100

var(sens_test)
sd(sens_test)
min(sens_test)
max(sens_test)
table(sens_test)

model$decision.values/1
model$accuracies
model$tot.accuracy
model$fitted


# Type of kernel function: 0 = linear: u'*v , 
# 1 = polynomial: (gamma*u'*v + coef0)^degree , 
# 2 = radial basis function: exp(-gamma*|u-v|^2)
######################################################################
###################################################################
####################################################################

# Datensatz 2 --> final_data_relevant_1_v4


dim(final_data_relevant_1_v4)
1/12

# http://www.di.fc.ul.pt/~jpn/r/svm/svm.html
weightss <- table(final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0)  # the weight vector must be named with the classes names
weightss[2] <- 5 # a class -1 mismatch has a terrible cost, 1 with 10 Zeros
weightss[1] <- 1    # a class +1 mismatch not so much...
# weightss

#set.seed(123)
# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()


# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()

#model


for (i in 1:1000) {
  
  training.samples <- final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- final_data_relevant_1_v4[training.samples, ]
  test.data <- final_data_relevant_1_v4[-training.samples, ]
  
  both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5)$data
  
  model = svm(formula = FRAUD_1_NONFRAUD_0 ~ .,
              data = both_train,
              type = 'C-classification',
              kernel = 'radial', class.weight=weightss, cost=0.75, gamma=0.04)
  
  # , cost = 1
  # , cost=0.1, class.weight=weightss, degree=3, , gamma=0.75
  # gamma = 0.1, coef=1 
  # kernel = 'polynomial', degree=3, , gamma=0.5
  # kernel = 'polynomial', degree=3,
  # cost = 5, gamma=0.5, class.weights=weightss
  predict_both <- predict(model, test.data, seed=1234)
  
  # TEST-DATEN - Sensitvität und Spezifität
  sens_t<-mean(predict_both[1:8] == test.data$FRAUD_1_NONFRAUD_0[1:8])
  spez_t<-mean(predict_both[9:25] == test.data$FRAUD_1_NONFRAUD_0[9:25])
  res_te<-mean(predict_both == test.data$FRAUD_1_NONFRAUD_0)
  
  sens_test<-c(sens_test, sens_t)
  spez_test<-c(spez_test, spez_t)
  test_res<-c(test_res, res_te)
  
  # TRAININGS-DATEN - Sensitvität und Spezifität
  speztr<-mean(both_train$FRAUD_1_NONFRAUD_0[1:31] == model$fitted[1:31])
  senstr<-mean(both_train$FRAUD_1_NONFRAUD_0[32:62] == model$fitted[32:62])
  res_tr<-mean(both_train$FRAUD_1_NONFRAUD_0 == model$fitted)
  
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
}

#length(predict_both)
#rf_both$forest
#table(both_train$FRAUD_1_NONFRAUD_0)
#dim(both_train)

#distinct(both_train[c(42:82),])

mean(sens_train)*100
mean(spez_train)*100
mean(train_res)*100

mean(sens_test)*100
mean(spez_test)*100
mean(test_res)*100



###################################################
##########################################################################
##############################################################################


