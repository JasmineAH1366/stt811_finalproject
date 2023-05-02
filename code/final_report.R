#STT 811 Project - Final Report 
#Diabetic Readmission Classification
#Suliah Apatira, Jasmine Harris, Raymond Lesiyon 
#05/02/2023

### Librareies
library(tidyverse)
library(ggcorrplot)
library(GGally)
library(naniar)
library(DT)
library(ggpubr)
library(pROC)
library(randomForest)
library(e1071)
library(MASS)
library(xgboost)
library(caret)

#METHODS/DATA PRE-PROCESSING 
diabetic_readmission_data <- read.csv("../data/diabetic_data_initial.csv")
data_size<- NROW(diabetic_readmission_data)

#FEATURE ENGINEERING 
## Missing values: 

dd <- diabetic_readmission_data %>% mutate(across(where(is.character), ~na_if(., "?")))
gg_miss_var(dd[, colSums(is.na(dd %>% mutate(across(where(is.character), ~na_if(., "?"))))) > 0] , show_pct=T) + 
  labs(x ="features", y = "Missing values in the dataset") + 
  theme_bw()

### Visualization of readmitted variable without/with hospice/expired patients removed

readmitted <- diabetic_readmission_data$readmitted
dead_hospice <- c(11,13,14,19,20,21,18,25,26)
### remove hospice/expire patients

diabetic_readmission_data <- diabetic_readmission_data %>%
  filter(!discharge_disposition_id %in% dead_hospice)

## visualizing without the expired/hospice patients remove.
gg1<- ggplot(data.frame(readmitted), aes(x=readmitted)) +
  geom_bar()
readmitted <- diabetic_readmission_data$readmitted
gg2<- ggplot(data.frame(readmitted), aes(x=readmitted)) +
  geom_bar()

figure <- ggarrange(gg1, gg2,
                    labels = c("a", "b"),
                    ncol = 2,nrow=1)
figure

### Data type-conversion
## The function below is used to convert to map the diag code to the coding used in the original paper

convert_diags <- function(diabetic_data) {
  for (col in c('diag_1', 'diag_2', 'diag_3')) {
    print(col)
    diabetic_data <- diabetic_data %>% 
      mutate(new_col = case_when(
        .data[[col]] %in%  c(390:459, 785) ~ 'Circulatory',
        .data[[col]] %in%  c(460:519, 786) ~ 'Respiratory',
        .data[[col]] %in%  c(520:579, 787) ~ 'Digestive',
        .data[[col]] %in%  c(800:999) ~ 'Injury',
        .data[[col]] %in%  c(710:739) ~ 'Musculoskeletal',
        .data[[col]] %in%  c(580:629, 788) ~ 'Genitourinary',
        .data[[col]] %in%  c(1:249, 251:279,680:709, 780:782, 784, 790:799, 782) ~ 'Neoplasms',
        startsWith(.data[[col]], '250' ) ~'Diabetes',
        .default = as.character('Other'))
      ) %>% mutate(!!col := new_col) %>% select(-new_col)
  }
  
  diabetic_data$diag_1 <- as.factor(diabetic_data$diag_1)
  diabetic_data$diag_2 <- as.factor(diabetic_data$diag_2)
  diabetic_data$diag_3 <- as.factor(diabetic_data$diag_3)
  return (diabetic_data)
}


## Univariate distributions
### Distributions for each numerical feature in the dataset

## Visualize medicines:
medicine1 <- c("metformin","repaglinide","nateglinide","chlorpropamide","glimepiride","acetohexamide","glipizide","glyburide","tolbutamide","pioglitazone","rosiglitazone","acarbose")

data<- diabetic_readmission_data[, medicine1]
col_names <- names(data)
nums_subplots <- length(medicine1)-1
col_s = 1
long_data <- pivot_longer(data[, seq(col_s:col_s+nums_subplots)], 
                          col_names[seq(col_s:col_s+nums_subplots)],
                          names_to="features",values_to="values")
g <- ggplot(long_data, aes(x=values, fill=features)) + 
  geom_histogram(stat='count') + 
  facet_wrap(~ features, scales = "free") + 
  ggtitle("Medicine Distribution") + coord_flip() + 
  scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
  theme(legend.position = "top", strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(g)

medicine2 <- c("miglitol","troglitazone","tolazamide","examide","citoglipton","insulin","glyburide.metformin","glipizide.metformin","glimepiride.pioglitazone","metformin.rosiglitazone","metformin.pioglitazone")

data<- diabetic_readmission_data[, medicine2]
col_names <- names(data)
nums_subplots <- length(medicine2)-1
col_s = 1
long_data <- pivot_longer(data[, seq(col_s:col_s+nums_subplots)], 
                          col_names[seq(col_s:col_s+nums_subplots)],
                          names_to="features",values_to="values")
g <- ggplot(long_data, aes(x=values, fill=features)) + 
  geom_histogram(stat='count') + 
  facet_wrap(~ features, scales = "free") + 
  ggtitle("Medicine Distribution") + coord_flip() + 
  scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
  theme(legend.position = "top", strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(g)

## encoding  readmission to 1, 0
diabetic_readmission_data$y <- ifelse(diabetic_readmission_data$readmitted %in% c("<30", ">30"), 1, 0)

## not log transformed data;
col_names <- c("num_lab_procedures", "num_procedures", "time_in_hospital", "num_medications", "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses", "y")
numeric_data <- diabetic_readmission_data[, col_names]
non_tranformed_corr <- ggcorrplot(cor(numeric_data))


#Removing weight, payer code, and medical specialty features
new_diabetic_readmission_data <- subset(diabetic_readmission_data, select = -c(weight, payer_code))
#new_diabetic_readmission_data #verifying those columns are gone

new_diabetic_readmission_data[new_diabetic_readmission_data$medical_specialty == "?", "medical_specialty"] <- "UNK"
### Encoding medical specialty
top_occuring_speciality <- names(head(sort(table(as.factor(new_diabetic_readmission_data$medical_specialty)), decreasing=T), n=10))
new_diabetic_readmission_data$medical_specialty <- as.factor(unlist(lapply(new_diabetic_readmission_data$medical_specialty, 
                                                                           function(x) ifelse(x %in%top_occuring_speciality, x, 'Other'))))

#code checking/removing missing values from diagnoses 3. I commented it out for now because I would like to discuss this with the group. 
#checking out many missing values are in diag_3 column 
sum(grepl("\\?", diabetic_readmission_data$diag_3))
sum(grepl("\\?", diabetic_readmission_data$diag_1))
sum(grepl("\\?", diabetic_readmission_data$diag_2))
length(diabetic_readmission_data$diag_3)

#1419 rows are missing from diag_3
#356 rows are missing from diag_2
#20 rows are missing from diag 1

#To remove the entire column: 
#new_diabetic_readmission_data <- subset(diabetic_readmission_data, select = -c(diag_3))

#To remove the rows:
new_diabetic_readmission_data <- new_diabetic_readmission_data[!grepl("\\?", new_diabetic_readmission_data$diag_3), ]
new_diabetic_readmission_data <- new_diabetic_readmission_data[!grepl("\\?", new_diabetic_readmission_data$diag_2), ]
new_diabetic_readmission_data <- new_diabetic_readmission_data[!grepl("\\?", new_diabetic_readmission_data$diag_1), ]
new_diabetic_readmission_data <- new_diabetic_readmission_data[!grepl("\\?", new_diabetic_readmission_data$race), ]
new_diabetic_readmission_data

#To remove race '?'
new_diabetic_readmission_data <- new_diabetic_readmission_data[!grepl("\\?", new_diabetic_readmission_data$race), ]
new_diabetic_readmission_data

sum(grepl("\\?", new_diabetic_readmission_data$diag_3))
sum(grepl("\\?", new_diabetic_readmission_data$diag_1))
sum(grepl("\\?", new_diabetic_readmission_data$diag_2))
sum(grepl("\\?", new_diabetic_readmission_data$race))

## log transformation data:
diabetic_readmission_transformations <- new_diabetic_readmission_data %>%
  mutate(log_time_in_hospital = log(time_in_hospital),
         log_num_lab_procedures = log(num_lab_procedures),
         log_num_procedures = log(num_procedures + 1),
         log_num_medications = log(num_medications),
         log_num_outpatient = log(number_outpatient + 1),
         log_num_emergency = log(number_emergency + 1),
         log_num_inpatient = log(number_inpatient + 1),
         log_num_diagnoses = log(number_diagnoses))


col_names <- c("log_time_in_hospital", "log_num_lab_procedures", "log_num_procedures", 
               "log_num_medications", "log_num_outpatient", "log_num_emergency", 
               "log_num_inpatient", "log_num_diagnoses", "y")
numeric_data <- diabetic_readmission_transformations[, col_names]
log_tranformed_corr <- ggcorrplot(cor(numeric_data))

## correlation plot for non-transformed and transformed data
print(non_tranformed_corr + ggtitle("Non transfoermed numerical data"))
print(log_tranformed_corr + ggtitle("Non transfoermed numerical data"))


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


## Processing the final data: making drug combinations
medicines <- c("metformin","repaglinide","nateglinide","chlorpropamide","glimepiride","acetohexamide",
               "glipizide","glyburide","tolbutamide","pioglitazone","rosiglitazone","acarbose", 
               "miglitol","troglitazone","tolazamide","examide","citoglipton","insulin",
               "glyburide.metformin","glipizide.metformin","glimepiride.pioglitazone",
               "metformin.rosiglitazone","metformin.pioglitazone")

removed_medicines <- c('examide','citoglipton', 'glimepiride.pioglitazone', 'metformin.rosiglitazone')

find_treatment_combination <- function(data) {
  medicine_col_names <- medicines[ !(medicines %in% removed_medicines)]
  combinations <- apply(data[,medicine_col_names], 1, function(row) {
    sorted_medicines <- sort(medicine_col_names[row == 1])
    paste0(sorted_medicines, collapse = "-")
  })
  return(combinations)
}

## Result to a lot of combinations; we regrouped the less occuring 
recategorize_drug_comb <- function(combinations) {
  combinations[combinations == ""] <- "no_medicine"
  return (as.factor(ifelse(combinations %in% names(which(table(as.factor(combinations)) > 900)),combinations, 'Other' )))
}

combinations <- find_treatment_combination(new_diabetic_readmission_data)
med_combs <- recategorize_drug_comb(combinations)

new_diabetic_readmission_data$med_combs <- med_combs

# removing drugs that in which less that 30 people took it.
new_diabetic_readmission_data <- new_diabetic_readmission_data[, !names(new_diabetic_readmission_data) %in% c("metformin.pioglitazone", 
                                                                                      "glipizide.metformin", 
                                                                                      "troglitazone", "acetohexamide", 
                                                                                      "tolbutamide", "glipizide.metformin", 
                                                                                      "tolazamide")]

## removing the encounter_id, patient_nbr, readmiited
new_diabetic_readmission_data <- new_diabetic_readmission_data[, col_names[!(col_names %in% c("encounter_id", 
                                                                                              "patient_nbr", 
                                                                                              "readmitted"))]]


#Final Data 
#After all the data pre-processing and feature engineering changes, we created a csv file for the final data.
final_data <- read.csv("../data/diabetic_bin_med_data_final.csv")

#MODELING 
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
auc_qda <- roc(qda_pred$class , test$y)$auc

###################################################################################################################
#XgBoost

col_names <- c("race", "age", "discharge_disposition_id", "admission_type_id", "admission_source_id",  "time_in_hospital", "medical_specialty","num_lab_procedures", "number_outpatient", "number_inpatient",  "number_emergency", "diag_1", "diag_2", "diag_3", "number_diagnoses", "A1Cresult", "metformin", "acarbose",  "insulin" , "change", "diabetesMed")

x_train <- train[, col_names]
x_test <- test[, col_names]

diabetic_readmission_model <- xgboost(data = data.matrix(x_train), 
                                      nrounds = 100, 
                                      max_depth = 2, 
                                      eta = 0.3, 
                                      label = data.matrix(train$y), objective = "binary:logistic")  

pred <- predict(diabetic_readmission_model, data.matrix(x_test))
confusionMatrix(as.factor(as.integer(2*pred)), as.factor(test$y))
print(auc(roc(test$y, pred)))

###################################################################################################################
  
#Linear Discriminant Analysis

readmission_lda_mod <- lda(data = trainData, y ~ num_lab_procedures + number_outpatient +number_inpatient+ number_emergency + number_diagnoses)

pred <- predict(readmission_lda_mod, test)
confusionMatrix(pred$class, as.factor(test$y))

print(auc(roc(test$y, pred$x)))

###################################################################################################################

#Analysis/Discussion 

### Most important features for xgboost
importance_matrix = xgb.importance(colnames(col_names), model = diabetic_readmission_model)
xgb.plot.importance(importance_matrix)