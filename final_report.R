#STT 811 Project - Final Report 
#Diabetic Readmission Classification
#Suliah Apatira, Jasmine Harris, Raymond Lesiyon 
#05/02/2023

#METHODS/DATA PRE-PROCESSING 

#FEATURE ENGINEERING 

#FE1 
new_diabetic_readmission_data$max_glu_serum <- as.factor(new_diabetic_readmission_data$max_glu_serum)
new_diabetic_readmission_data$A1Cresult <- as.factor(new_diabetic_readmission_data$A1Cresult)
new_diabetic_readmission_data$metformin <- as.factor(new_diabetic_readmission_data$metformin)
new_diabetic_readmission_data$repaglinide <- as.factor(new_diabetic_readmission_data$repaglinide)
new_diabetic_readmission_data$nateglinide <- as.factor(new_diabetic_readmission_data$nateglinide)
new_diabetic_readmission_data$chlorpropamide <- as.factor(new_diabetic_readmission_data$chlorpropamide)
new_diabetic_readmission_data$glimepiride <- as.factor(new_diabetic_readmission_data$glimepiride)
new_diabetic_readmission_data$acetohexamide <- as.factor(new_diabetic_readmission_data$acetohexamide)
new_diabetic_readmission_data$glipizide <- as.factor(new_diabetic_readmission_data$glipizide)
new_diabetic_readmission_data$glyburide <- as.factor(new_diabetic_readmission_data$glyburide)
new_diabetic_readmission_data$tolbutamide <- as.factor(new_diabetic_readmission_data$tolbutamide)
new_diabetic_readmission_data$pioglitazone <- as.factor(new_diabetic_readmission_data$pioglitazone)
new_diabetic_readmission_data$rosiglitazone <- as.factor(new_diabetic_readmission_data$rosiglitazone)
new_diabetic_readmission_data$acarbose <- as.factor(new_diabetic_readmission_data$acarbose)
new_diabetic_readmission_data$miglitol <- as.factor(new_diabetic_readmission_data$miglitol)
new_diabetic_readmission_data$troglitazone <- as.factor(new_diabetic_readmission_data$troglitazone)
new_diabetic_readmission_data$tolazamide <- as.factor(new_diabetic_readmission_data$tolazamide)
new_diabetic_readmission_data$examide <- as.factor(new_diabetic_readmission_data$examide)
new_diabetic_readmission_data$citoglipton <- as.factor(new_diabetic_readmission_data$citoglipton)
new_diabetic_readmission_data$insulin <- as.factor(new_diabetic_readmission_data$insulin)
new_diabetic_readmission_data$glyburide.metformin <- as.factor(new_diabetic_readmission_data$glyburide.metformin)
new_diabetic_readmission_data$glipizide.metformin <- as.factor(new_diabetic_readmission_data$glipizide.metformin)
new_diabetic_readmission_data$glimepiride.pioglitazone <- as.factor(new_diabetic_readmission_data$glimepiride.pioglitazone)
new_diabetic_readmission_data$metformin.rosiglitazone <- as.factor(new_diabetic_readmission_data$metformin.rosiglitazone)
new_diabetic_readmission_data$metformin.pioglitazone <- as.factor(new_diabetic_readmission_data$metformin.pioglitazone)

#FE2 - max_glu_serum 
table(new_diabetic_readmission_data$max_glu_serum)

#Binarize
new_diabetic_readmission_data$max_glu_serum <- as.factor(ifelse(new_diabetic_readmission_data$max_glu_serum == ">200" 
                                                                |new_diabetic_readmission_data$max_glu_serum == ">300" 
                                                                | new_diabetic_readmission_data$max_glu_serum == "Norm",1,0))

#FE3 - A1CResult 
table(new_diabetic_readmission_data$A1Cresult)

#Binarize
new_diabetic_readmission_data$A1Cresult <- as.factor(ifelse(new_diabetic_readmission_data$A1Cresult == ">7" |new_diabetic_readmission_data$A1Cresult == ">8" 
                                                            | new_diabetic_readmission_data$A1Cresult == "Norm",1,0))

#FE4 - Medicines - Total 4 
table(new_diabetic_readmission_data$examide)
table(new_diabetic_readmission_data$citoglipton)
table(new_diabetic_readmission_data$glimepiride.pioglitazone)
table(new_diabetic_readmission_data$metformin.rosiglitazone)

#Deleting above 4 Medications 
new_diabetic_readmission_data <- new_diabetic_readmission_data[,!names(new_diabetic_readmission_data) %in% c('examide','citoglipton', 'glimepiride.pioglitazone', 'metformin.rosiglitazone')]

#FE5 - Medicines - Total 6 
table(new_diabetic_readmission_data$acetohexamide) #Only 1 patient received this medication 
table(new_diabetic_readmission_data$tolbutamide) #16 patients received this medication 
table(new_diabetic_readmission_data$troglitazone) #2 patients received this medication
table(new_diabetic_readmission_data$tolazamide) #24 patients received this medication
table(new_diabetic_readmission_data$glipizide.metformin) #7 patients received this medication
table(new_diabetic_readmission_data$metformin.pioglitazone) #Only 1 patient received this medication

#Deleting above 6 Medications
diabetic_bin_med_data <- diabetic_bin_med_data[, !names(diabetic_bin_med_data) %in% c("metformin.pioglitazone", "troglitazone", "acetohexamide", "tolbutamide", "glipizide.metformin", "tolazamide")]

#FE6 - Medicines - Other 
table(new_diabetic_readmission_data$acarbose)
table(new_diabetic_readmission_data$metformin)
table(new_diabetic_readmission_data$repaglinide)
table(new_diabetic_readmission_data$nateglinide)
table(new_diabetic_readmission_data$chlorpropamide)
table(new_diabetic_readmission_data$glimepiride)
table(new_diabetic_readmission_data$glipizide)
table(new_diabetic_readmission_data$glyburide)
table(new_diabetic_readmission_data$pioglitazone)
table(new_diabetic_readmission_data$rosiglitazone)
table(new_diabetic_readmission_data$miglitol)
table(new_diabetic_readmission_data$insulin)
table(new_diabetic_readmission_data$glyburide.metformin)

#Binarize
new_diabetic_readmission_data$acarbose <- as.factor(ifelse(new_diabetic_readmission_data$acarbose == "Down"| new_diabetic_readmission_data$acarbose == "Steady" | new_diabetic_readmission_data$acarbose == "Up",1,0))
new_diabetic_readmission_data$metformin <- as.factor(ifelse(new_diabetic_readmission_data$metformin == "Down"| new_diabetic_readmission_data$metformin == "Steady" | new_diabetic_readmission_data$metformin == "Up",1,0))
new_diabetic_readmission_data$repaglinide <- as.factor(ifelse(new_diabetic_readmission_data$repaglinide == "Down"| new_diabetic_readmission_data$repaglinide == "Steady" | new_diabetic_readmission_data$repaglinide == "Up",1,0))
new_diabetic_readmission_data$nateglinide <- as.factor(ifelse(new_diabetic_readmission_data$nateglinide == "Down"| new_diabetic_readmission_data$nateglinide == "Steady" | new_diabetic_readmission_data$nateglinide == "Up",1,0))
new_diabetic_readmission_data$chlorpropamide <- as.factor(ifelse(new_diabetic_readmission_data$chlorpropamide == "Down"| new_diabetic_readmission_data$chlorpropamide == "Steady" | new_diabetic_readmission_data$chlorpropamide == "Up",1,0))
new_diabetic_readmission_data$glimepiride <- as.factor(ifelse(new_diabetic_readmission_data$glimepiride == "Down"| new_diabetic_readmission_data$glimepiride == "Steady" | new_diabetic_readmission_data$glimepiride == "Up",1,0))
new_diabetic_readmission_data$glipizide <- as.factor(ifelse(new_diabetic_readmission_data$glipizide == "Down"| new_diabetic_readmission_data$glipizide == "Steady" | new_diabetic_readmission_data$glipizide == "Up",1,0))
new_diabetic_readmission_data$glyburide <- as.factor(ifelse(new_diabetic_readmission_data$glyburide == "Down"| new_diabetic_readmission_data$glyburide == "Steady" | new_diabetic_readmission_data$glyburide == "Up",1,0))
new_diabetic_readmission_data$pioglitazone <- as.factor(ifelse(new_diabetic_readmission_data$pioglitazone == "Down"| new_diabetic_readmission_data$pioglitazone == "Steady" | new_diabetic_readmission_data$pioglitazone == "Up",1,0))
new_diabetic_readmission_data$rosiglitazone <- as.factor(ifelse(new_diabetic_readmission_data$rosiglitazone == "Down"| new_diabetic_readmission_data$rosiglitazone == "Steady" | new_diabetic_readmission_data$rosiglitazone == "Up",1,0))
new_diabetic_readmission_data$miglitol <- as.factor(ifelse(new_diabetic_readmission_data$miglitol == "Down"| new_diabetic_readmission_data$miglitol == "Steady" | new_diabetic_readmission_data$miglitol == "Up",1,0))
new_diabetic_readmission_data$insulin <- as.factor(ifelse(new_diabetic_readmission_data$insulin == "Down"| new_diabetic_readmission_data$insulin == "Steady" | new_diabetic_readmission_data$insulin == "Up",1,0))
new_diabetic_readmission_data$glyburide.metformin <- as.factor(ifelse(new_diabetic_readmission_data$glyburide.metformin == "Down"| new_diabetic_readmission_data$glyburide.metformin == "Steady" | new_diabetic_readmission_data$glyburide.metformin == "Up",1,0))

#MODELING 

#Final Data 
#After all the data pre-processing and feature engineering changes, we created a csv file for the final data.
final_data <- read.csv("..\data\diabetic_bin_med_data_final.csv")

#Test/Train Split 
split_pct <- 0.7
n <- length(final_data$y)*split_pct 
row_samp <- sample(1:length(final_data$y), n, replace = FALSE)
train <- final_data[row_samp,]
test <- final_data[-row_samp,]

###################################################################################################################
  
#Logistic Regression - Significant Features - LG1
log_mod <- glm(data = train, y ~ ., family = binomial)
pred <- predict(log_mod, newdata = test, type = "response")
pred_class <- ifelse(pred >= 0.5, 1, 0)

# confusion matrix
cm <- table(pred_class, test$y)

#finding which p values are significant 
p_values <- summary(log_mod)$coefficients[, "Pr(>|z|)"]
var_names <- names(p_values)

# identify significant predictors
sig_vars <- var_names[p_values < 0.05]

#accuracy
mean(pred_class == test$y)

###################################################################################################################

#Logistic Regression - Baseline Model - LG2
log_mod <- glm(data = train, y ~ number_inpatient, family = binomial)
pred <- predict(log_mod, newdata = test, type = "response")
pred_class2 <- ifelse(pred >= 0.5, 1, 0)

# confusion matrix
cm2 <- table(pred_class, test$y)


#Accuracy
mean(pred_class2 == test$y)
cm

#Area Under Cruve
auc_log <- roc(test$y, pred)$auc
auc_log

###################################################################################################################

#Logistic Regression - LG3
log_mod <- glm(data = train, y ~ race +  age + discharge_disposition_id + admission_type_id + admission_source_id + time_in_hospital + medical_specialty + num_lab_procedures + number_outpatient + number_inpatient + number_emergency + diag_1 + diag_2 + diag_3 + number_diagnoses + A1Cresult + metformin + acarbose + insulin + change + diabetesMed, family = binomial)
pred2 <- predict(log_mod, newdata = test, type = "response")
pred_class3 <- ifelse(pred2 >= 0.5, 1, 0)

# confusion matrix
cm2 <- table(pred_class3, test$y)
#accuracy
mean(pred_class3 == test$y)
cm2

#Area Under Curve
auc_log <- roc(test$y, pred)$auc
auc_log

###################################################################################################################

#Random Forest 

#RF Model 
readmission_rf <- randomForest(as.factor(y) ~ race + age + discharge_disposition_id + admission_type_id + admission_source_id 
                               + time_in_hospital + medical_specialty + num_lab_procedures + number_outpatient + number_inpatient 
                               + number_emergency + diag_1 + diag_2 + diag_3 + number_diagnoses + A1Cresult + metformin + acarbose 
                               + insulin + change + diabetesMed , data = train, mtry = 1, importance = TRUE, ntree = 1000, maxnodes = 4)

#RF Accuracy 
rf_predict <- predict(readmission_rf, test)
rf_table <- table(rf_predict, test$y)
rf_table

accuracy_test_rf <- sum(diag(rf_table))/sum(rf_table)
accuracy_test_rf

###################################################################################################################

# Naive Bayes - NB Model
nbModel <- naiveBayes(y ~ race +  age + discharge_disposition_id + admission_type_id + admission_source_id + time_in_hospital + medical_specialty + num_lab_procedures + number_outpatient + number_inpatient + number_emergency + diag_1 + diag_2 + diag_3 + number_diagnoses + A1Cresult + metformin + acarbose + insulin + change + diabetesMed, data = train, method = "nb")
predictions <- predict(nbModel, newdata = test)

# Accuracy 
confusionMatrix(predictions, test$y)
mean(predictions == test$y)

#Area under curve
predictions_nb <- cbind(1-predictions_nb, predictions_nb)
roc_obj <- roc(test$y, predictions_nb)
auc(roc_obj)
auc_nb <- roc(test$y, predictions_nb[,2])$auc

###################################################################################################################
  
#Quadratic Discriminant Analysis - QDA Model
qda_mod <- qda(y ~  num_lab_procedures + number_outpatient + number_inpatient+number_emergency+number_diagnoses, data=train)
qda_pred <- predict(qda_mod, newdata=test)
table_preds <- table(qda_pred$class, test$y)

#Accuracy
accuracy <- sum(diag(table_preds)) / sum(table_preds) * 100
accuracy
#Area Under Curve
auc_qda <- roc(table_preds , test2$y)$auc

###################################################################################################################
#XgBoost

  
###################################################################################################################
  
#Linear Discriminant Analysis

  
###################################################################################################################

  
#Analysis/Discussion 
