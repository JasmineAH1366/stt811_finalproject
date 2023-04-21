### All Medicines: 
#medicines <- c("glipizide", "insulin", "metformin", "glyburide", 
#               "glyburide.metformin", "pioglitazone", "repaglinide", 
#               "nateglinide", "rosiglitazone", "acarbose", "glimepiride")

medicines <- c("metformin","repaglinide","nateglinide","chlorpropamide","glimepiride","acetohexamide",
               "glipizide","glyburide","tolbutamide","pioglitazone","rosiglitazone","acarbose", 
               "miglitol","troglitazone","tolazamide","examide","citoglipton","insulin",
               "glyburide.metformin","glipizide.metformin","glimepiride.pioglitazone",
               "metformin.rosiglitazone","metformin.pioglitazone")

removed_medicines <- c('examide','citoglipton', 'glimepiride.pioglitazone', 'metformin.rosiglitazone')
discontinued_medicines <- c("tolbutamide", "pioglitazone", "rosiglitazone", "troglitazone", "tolazamide")

remove_unbalanced_categorical_values <- function(data, lowest) {
  col_names <- names(data)[sapply(data, is.numeric)]
  
  factor_names <- names(data)[unlist(lapply(data, is.factor))]
  
  factor_counts <- lapply(data[, factor_names, drop = FALSE], table)
  factor_names <- factor_names[sapply(factor_counts, function(x) all(x >= lowest))]
  
  return ( data[, c(col_names, factor_names)] )
} 

### remove discontinued drugs
treated_with_banned <- function(data, medicine_col_names) {
  row <- rowSums(data[, medicine_col_names] == 1)
  return (as.factor(ifelse(row==0, 0, 1)))
}

## patients treated with any drugs
patients_treated_with_any_drugs <- function(data, medicine_col_names){
  medicine_col_names <- medicine_col_names[ !(medicine_col_names %in% removed_medicines)]
  row <- rowSums(data[, medicine_col_names] == 1)
  row <- ifelse(row==0, 0, 1)
  return (as.factor(ifelse(row==0, 0, 1)))
}

## getting the med combinations (All the medicines that were either Steady, Up, Down)
find_treatment_combination <- function(data) {
  medicine_col_names <- medicines[ !(medicines %in% removed_medicines)]
  combinations <- apply(data[,medicine_col_names], 1, function(row) {
    sorted_medicines <- sort(medicine_col_names[row == 1])
    paste0(sorted_medicines, collapse = "-")
  })
  return(combinations)
}

## recategorize the drugs 
recategorize_drug_comb <- function(combinations) {
  combinations[combinations == ""] <- "no_medicine"
  return (as.factor(ifelse(combinations %in% names(which(table(as.factor(combinations)) > 900)),combinations, 'Other' )))
}

### Convert the diag_1, diag_2, diag_3 into respective columns:

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




