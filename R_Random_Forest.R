

#############################################
## Random Forest


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




# Random Forest für final_data_relevant_1_v4 --> Datensatz 2

# Test Versuche über Anzahl von Trees

spezifs<-c()
sensis<-c()
for (i in seq(from=100, to=5000, by=100)) {
  rf <- randomForest(FRAUD_1_NONFRAUD_0~., data=final_data_relevant_1_v4, proximity=TRUE,
                     ntree = i) 
  
  spez<-(1-rf$confusion[5])
  sens<-(1-rf$confusion[6])
  
  spezifs<-c(spezifs, spez)
  sensis<-c(sensis, sens)
  
}

summary((spezifs*59 + sensis*28)/87)

summary(sensis)
summary(spezifs)


########################################################
## Random Forest
# Aufteilung der NON-Fraud Daten in zwei Datensätze:

# Cross-Validation 1000 mal - mit getrennten Datensatz - Lösung Ungleichverteilung
# NON-FRaud DATENSATZ wird in LOOP 1000x immer wieder neu gemischt
# wodurch Random-Forest immer wieder 

# Trennung Daten in FRAUD und nicht-FRAUD
fraud_data<-final_data_relevant_1_v4[1:28,]
nonfraud_data<-final_data_relevant_1_v4[29:87,]

# Sensitivität und Spezifität von Datensatz 1 
spezifs1<-c()
sensis1<-c()
# Sensitivität und Spezifität von Datensatz 2 
spezifs2<-c()
sensis2<-c()

for (i in 1:1000) {
  
  # Aufteilung NICHT-FRAUD 50/50 Teile 
  data.samples <- nonfraud_data$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.5, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  non_fraud_1  <- nonfraud_data[data.samples, ]
  non_fraud_2 <- nonfraud_data[-data.samples, ]
  
  # zusammenfügen der Datensätze: 
  data_1 <- rbind(fraud_data, non_fraud_1)
  data_2 <- rbind(fraud_data, non_fraud_2)
  
  
  rf_1 <- randomForest(FRAUD_1_NONFRAUD_0~., data=data_1, proximity=TRUE,
                       ntree = 2000)
  
  rf_2 <- randomForest(FRAUD_1_NONFRAUD_0~., data=data_2, proximity=TRUE,
                       ntree = 2000)
  
  spez1<-(1-rf_1$confusion[5])
  sens1<-(1-rf_1$confusion[6])
  
  spez2<-(1-rf_2$confusion[5])
  sens2<-(1-rf_2$confusion[6])
  
  spezifs1<-c(spezifs1, spez1)
  sensis1<-c(sensis1, sens1)
  
  
  spezifs2<-c(spezifs2, spez2)
  sensis2<-c(sensis2, sens2)
  
}
dim(data_1)

59/2

summary((spezifs1*30 + sensis1*28)/58)

summary((spezifs2*29 + sensis2*28)/57)

summary(spezifs1)
summary(sensis1)

summary(spezifs2)
summary(sensis2)



################################################

# Lösung für ungleichverteilte Datensatz mittels over/undersampling


# erster Versuch: over/under sampling
library(ROSE)

#Undersampling and Oversampling

final_data_relevant_1_v4
?ovun.sample 
# Creates possibly balanced samples by random over-sampling minority examples
# The desired sample size of the resulting data set.
# If missing and method is either "over" or "under" the sample size is determined by oversampling or, 
# respectively, undersampling examples so that the minority class occurs approximately in proportion p.
# method= over, under, both - oversampling, undersampling or both
# Verhältnis je Gruppe p = 0.5 =50/50 Aufteilung 


over_sample <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=final_data_relevant_1_v4, method = "over", p= 0.5,seed=1234)$data
# table(over_sample$FRAUD_1_NONFRAUD_0) # 0: 59 und 1: 50 = 109 Instances

# ?randomForest
both <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=final_data_relevant_1_v4, method = "both", p= 0.5,seed=1234)$data
table(both$FRAUD_1_NONFRAUD_0) # 0: 46 1: 41  = 87 Instances

dim(both) #  87 13
dim(distinct(both)) # 57 13

rf_both = randomForest(FRAUD_1_NONFRAUD_0~., data = over_sample)

p2Both <- predict(rfBoth, validSIAM, seed=1234)



rfboth = randomForest(FRAUD_1_NONFRAUD_0~., data = final_data_relevant_1_v4, classwt = c(1,2),
                      proximity=TRUE,
                      ntree = 2000,
                      cutoff = c(0.7,0.3))
rf





#####################################################################
#### 


# Random-Forest - Versuch 1:
# both:

classwt<-seq(from=0.25, to=20, by=0.25)
sens_train<-c()
spez_train<-c()
train_res<-c()

for (i in seq(from=0.25, to=20, by=0.25)) {
  rf_both<-randomForest(FRAUD_1_NONFRAUD_0~.,data = final_data_relevant_1_v4, 
                        ntree=1000, replace = TRUE, 
                        classwt = c(i,1))
  
  speztr<-(1-rf_both$confusion[5])
  senstr<-(1-rf_both$confusion[6])
  res_tr<-((1-rf_both$confusion[5])+ (1-rf_both$confusion[6]))/2
  
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
  
}

max(sens_train)
classwt[28]
which.max(sens_train)
sens_train
spez_train
mean()
mean(sens_train)



# Cross-Validierung Random-Forest

# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()

# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()

table(final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0)


#dim(final_data_relevant_1_v4)

for (i in 1:1000) {
  
  training.samples <- final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- final_data_relevant_1_v4[training.samples, ]
  test.data <- final_data_relevant_1_v4[-training.samples, ]
  
  both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5,seed=1234)$data
  
  # cutoff = c(0.63,0.37)
  rf_both = randomForest(FRAUD_1_NONFRAUD_0~., data = both_train, 
                         ntree=1000, mtry=12,
                         cutoff = c(0.63, 0.37), 
                         classwt = c(1, 17.25))
  
  predict_both <- predict(rf_both, test.data, seed=1234)
  
  # TEST-DATEN - Sensitvität und Spezifität
  sens_t<-mean(predict_both[1:8] == test.data$FRAUD_1_NONFRAUD_0[1:8])
  spez_t<-mean(predict_both[9:25] == test.data$FRAUD_1_NONFRAUD_0[9:25])
  res_te<-mean(predict_both == test.data$FRAUD_1_NONFRAUD_0)
  
  sens_test<-c(sens_test, sens_t)
  spez_test<-c(spez_test, spez_t)
  test_res<-c(test_res, res_te)
  
  # TRAININGS-DATEN - Sensitvität und Spezifität
  speztr<-(1-rf_both$confusion[5])
  senstr<-(1-rf_both$confusion[6])
  res_tr<-((1-rf_both$confusion[5])+ (1-rf_both$confusion[6]))/2
  
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
}

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

dim(train.data)
dim(both_train)




########################################################
# Versuch mit anderen Datensatz: final_data_relevant_1_v2 --> Datensatz 1

# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()

# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()

#dim(final_data_relevant_1_v4)
#?randomForest.formula

#showMethods("randomForest")

for (i in 1:1000) {
  
  training.samples <- final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- final_data_relevant_1_v2[training.samples, ]
  test.data <- final_data_relevant_1_v2[-training.samples, ]
  
  both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5,seed=1234)$data
  
  # cutoff = c(0.63,0.37)
  rf_both = randomForest(FRAUD_1_NONFRAUD_0~., data = both_train, 
                         ntree=1000, mtry=12,
                         cutoff=c(0.7, 0.3), classwt=c(1,17.25))
  
  # cutoff=c(0.63, 0.37)
  # classwt = c(0.0008, 1000) classwt=c(1,100)
  predict_both <- predict(rf_both, test.data, seed=1234)
  
  # TEST-DATEN - Sensitvität und Spezifität
  sens_t<-mean(predict_both[1:8] == test.data$FRAUD_1_NONFRAUD_0[1:8])
  spez_t<-mean(predict_both[9:25] == test.data$FRAUD_1_NONFRAUD_0[9:25])
  res_te<-mean(predict_both == test.data$FRAUD_1_NONFRAUD_0)
  
  sens_test<-c(sens_test, sens_t)
  spez_test<-c(spez_test, spez_t)
  test_res<-c(test_res, res_te)
  
  # TRAININGS-DATEN - Sensitvität und Spezifität
  speztr<-(1-rf_both$confusion[5])
  senstr<-(1-rf_both$confusion[6])
  res_tr<-((1-rf_both$confusion[5])+ (1-rf_both$confusion[6]))/2
  
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
}

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

colnames(final_data_relevant_1_v2)



###################################

# Test ohne over-undersampling


# Test-Ergebnisse:
sens_test<-c()
spez_test<-c()
test_res<-c()

# Trainings-Ergebnisse:
sens_train<-c()
spez_train<-c()
train_res<-c()

#dim(final_data_relevant_1_v4)
#?randomForest.formula

table(final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0)

for (i in 1:100) {
  
  training.samples <- final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- final_data_relevant_1_v2[training.samples, ]
  test.data <- final_data_relevant_1_v2[-training.samples, ]
  
  #both_train <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "both", p= 0.5,seed=1234)$data
  
  # cutoff = c(0.63,0.37)
  rf_both = randomForest(FRAUD_1_NONFRAUD_0~., data = train.data, 
                         ntree=1000, mtry=12,
                         cutoff=c(0.8,0.2), classwt=c())
  
  # cutoff=c(0.63, 0.37) , classwt=c(50,1)
  # classwt = c(0.0008, 1000) classwt=c(1,100)
  predict_both <- predict(rf_both, test.data, seed=1234)
  
  # TEST-DATEN - Sensitvität und Spezifität
  sens_t<-mean(predict_both[1:8] == test.data$FRAUD_1_NONFRAUD_0[1:8])
  spez_t<-mean(predict_both[9:25] == test.data$FRAUD_1_NONFRAUD_0[9:25])
  res_te<-mean(predict_both == test.data$FRAUD_1_NONFRAUD_0)
  
  sens_test<-c(sens_test, sens_t)
  spez_test<-c(spez_test, spez_t)
  test_res<-c(test_res, res_te)
  
  # TRAININGS-DATEN - Sensitvität und Spezifität
  speztr<-(1-rf_both$confusion[5])
  senstr<-(1-rf_both$confusion[6])
  res_tr<-((1-rf_both$confusion[5])+ (1-rf_both$confusion[6]))/2
  
  spez_train<-c(spez_train, speztr)
  sens_train<-c(sens_train, senstr)
  train_res<-c(train_res, res_tr)
  
}

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




