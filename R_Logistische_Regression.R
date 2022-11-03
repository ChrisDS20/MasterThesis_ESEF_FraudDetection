

#######################################

# Logistische Regression:

# Datensatz 1 - nur signifikante Variablen

final_data_relevant_1_v4 <- final_data_relevant_1_v4[,c("FRAUD_1_NONFRAUD_0", "DSRI", "PMR_4", "DEF_TAX_EXP_VJ", 
                                                        "ZSCORE_4_J", "ZSCORE_4_VJ", "POS_NEG_O_CF_J", "POS_NEG_O_CF_VJ",
                                                        "J_var3_VJ", "MJ_var2_J", "MJ_var2_4", "entity_specific_tag", "issue_sum")]

colnames(final_data_relevant_1_v4)
final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0)
final_data_relevant_1_v4$POS_NEG_O_CF_J <- as.factor(final_data_relevant_1_v4$POS_NEG_O_CF_J)
final_data_relevant_1_v4$POS_NEG_O_CF_VJ <- as.factor(final_data_relevant_1_v4$POS_NEG_O_CF_VJ)
final_data_relevant_1_v4$issue_sum <- as.factor(final_data_relevant_1_v4$issue_sum)
str(final_data_relevant_1_v4)
final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0)

set.seed(123)
sens_results<-c()
spec_results<-c()
results_test<-c()


sens_results_train<-c()
spec_results_train<-c()
results_train<-c()

#?glm
Grenzwert <- 0.5 # Eingabe Grenzwert
for (i in 1:1000) {
  
  training.samples <- final_data_relevant_1_v4$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- final_data_relevant_1_v4[training.samples, ]
  test.data <- final_data_relevant_1_v4[-training.samples, ]
  
  both <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "over", p= 0.5,seed=1234)$data
  
  # Fit the model
  model <- glm( FRAUD_1_NONFRAUD_0 ~ ., 
                data = both, family = binomial)
  
  predicted_train <- predict(model, train.data, type="response")
  predicted_test <- predict(model, test.data, type="response")
  round(predicted_test, 2)
  summary(model)
  
  predicted_result<-ifelse(predicted_test > Grenzwert, 1, 0)
  predicted_result_train<-ifelse(predicted_train > Grenzwert, 1, 0)
  
  result<-mean(predicted_result == test.data$FRAUD_1_NONFRAUD_0)
  results_test<-c(results_test, result)
  
  result_train<-mean(predicted_result_train == train.data$FRAUD_1_NONFRAUD_0)
  results_train<-c(results_train, result_train)
  
  
  # Ermittlung des Spezifität und Sensitivität TEST
  actual_values<-test.data$FRAUD_1_NONFRAUD_0
  conf_matrix<-table(predicted_result,actual_values)
  sens<-conf_matrix[4]/(conf_matrix[3]+conf_matrix[4])
  spec<-conf_matrix[1]/(conf_matrix[1]+conf_matrix[2])
  
  
  # Spezifität, Sensitivität TRAIN
  train_values<-train.data$FRAUD_1_NONFRAUD_0
  conf_matrix_tr<-table(predicted_result_train,train_values)
  sens_train<-conf_matrix_tr[4]/(conf_matrix_tr[3]+conf_matrix_tr[4])
  spec_train<-conf_matrix_tr[1]/(conf_matrix_tr[1]+conf_matrix_tr[2])
  
  # Einzelergebnisse in die Vektoren einfügen
  sens_results<-c(sens_results, sens)
  spec_results<-c(spec_results, spec)
  
  sens_results_train<-c(sens_results_train, sens_train)
  spec_results_train<-c(spec_results_train, spec_train)
  
  
}

sens_results[is.na(sens_results)]<-0
spec_results[is.na(spec_results)]<-0

sens_results_train[is.na(sens_results_train)]<-0
spec_results_train[is.na(spec_results_train)]<-0

mean(sens_results)*100
mean(spec_results)*100
mean(results_test)*100

mean(sens_results_train)*100
mean(spec_results_train)*100
mean(results_train)*100

round(predicted_test, 2)

summary(model)



# Datensatz 1 - nicht signifikante Variablen

final_data_relevant_1_v2<-final_data_relevant_1[, names(final_data_relevant_1) %in% 
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


colnames(final_data_relevant_1_v2)
final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0)
final_data_relevant_1_v2$POS_NEG_O_CF_J <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_J)
final_data_relevant_1_v2$POS_NEG_O_CF_VJ <- as.factor(final_data_relevant_1_v2$POS_NEG_O_CF_VJ)
final_data_relevant_1_v2$issue_sum <- as.factor(final_data_relevant_1_v2$issue_sum)
str(final_data_relevant_1_v2)
final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0 <- as.factor(final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0)



set.seed(123)
sens_results<-c()
spec_results<-c()
results_test<-c()


sens_results_train<-c()
spec_results_train<-c()
results_train<-c()

#?glm
Grenzwert <- 0.5
for (i in 1:1000) {
  
  training.samples <- final_data_relevant_1_v2$FRAUD_1_NONFRAUD_0 %>% 
    createDataPartition(p = 0.7, list = FALSE)
  # Zuweisung der Indexwerte zu Train und Test
  train.data  <- final_data_relevant_1_v2[training.samples, ]
  test.data <- final_data_relevant_1_v2[-training.samples, ]
  
  both <- ovun.sample(FRAUD_1_NONFRAUD_0~., data=train.data, method = "over", p= 0.5,seed=1234)$data
  
  # Fit the model
  model <- glm( FRAUD_1_NONFRAUD_0 ~ ., 
                data = both, family = binomial)
  
  predicted_train <- predict(model, train.data, type="response")
  predicted_test <- predict(model, test.data, type="response")
  round(predicted_test, 2)
  summary(model)
  
  predicted_result<-ifelse(predicted_test > Grenzwert, 1, 0)
  predicted_result_train<-ifelse(predicted_train > Grenzwert, 1, 0)
  
  result<-mean(predicted_result == test.data$FRAUD_1_NONFRAUD_0)
  results_test<-c(results_test, result)
  
  result_train<-mean(predicted_result_train == train.data$FRAUD_1_NONFRAUD_0)
  results_train<-c(results_train, result_train)
  
  
  # Ermittlung des Spezifität und Sensitivität TEST
  actual_values<-test.data$FRAUD_1_NONFRAUD_0
  conf_matrix<-table(predicted_result,actual_values)
  sens<-conf_matrix[4]/(conf_matrix[3]+conf_matrix[4])
  spec<-conf_matrix[1]/(conf_matrix[1]+conf_matrix[2])
  
  
  # Spezifität, Sensitivität TRAIN
  train_values<-train.data$FRAUD_1_NONFRAUD_0
  conf_matrix_tr<-table(predicted_result_train,train_values)
  sens_train<-conf_matrix_tr[4]/(conf_matrix_tr[3]+conf_matrix_tr[4])
  spec_train<-conf_matrix_tr[1]/(conf_matrix_tr[1]+conf_matrix_tr[2])
  
  # Einzelergebnisse in die Vektoren einfügen
  sens_results<-c(sens_results, sens)
  spec_results<-c(spec_results, spec)
  
  sens_results_train<-c(sens_results_train, sens_train)
  spec_results_train<-c(spec_results_train, spec_train)
  
  
}

sens_results[is.na(sens_results)]<-0
spec_results[is.na(spec_results)]<-0

sens_results_train[is.na(sens_results_train)]<-0
spec_results_train[is.na(spec_results_train)]<-0

mean(sens_results)*100
mean(spec_results)*100
mean(results_test)*100

mean(sens_results_train)*100
mean(spec_results_train)*100
mean(results_train)*100

round(predicted_test, 2)

summary(model)
