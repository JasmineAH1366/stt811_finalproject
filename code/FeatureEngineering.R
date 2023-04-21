print("running here")
#Changing categorical variables into numeric 
new_diabetic_readmission_data$race <- as.factor(new_diabetic_readmission_data$race)
new_diabetic_readmission_data$gender <- as.factor(new_diabetic_readmission_data$gender)
new_diabetic_readmission_data$age <- as.factor(new_diabetic_readmission_data$age)
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
new_diabetic_readmission_data$change <- as.factor(new_diabetic_readmission_data$change)
new_diabetic_readmission_data$diabetesMed <- as.factor(new_diabetic_readmission_data$diabetesMed)
new_diabetic_readmission_data$readmitted <- as.factor(new_diabetic_readmission_data$readmitted)


#removing unknown/invalid
new_diabetic_readmission_data <- new_diabetic_readmission_data[!grepl("Unknown/Invalid", new_diabetic_readmission_data$gender), ]


#gender
new_diabetic_readmission_data$gender <- as.factor(ifelse(new_diabetic_readmission_data$gender == "Male",1,0))

#age
table(new_diabetic_readmission_data$age)

#max_glu_serum
table(new_diabetic_readmission_data$max_glu_serum)

#Binarizing Variables - MAX_GLU_SERUM 

#gender
new_diabetic_readmission_data$max_glu_serum <- as.factor(ifelse(new_diabetic_readmission_data$max_glu_serum == ">200" | new_diabetic_readmission_data$max_glu_serum == ">300" | new_diabetic_readmission_data$max_glu_serum == "Norm",1,0))
table(new_diabetic_readmission_data$max_glu_serum)

#A1Cresult
table(new_diabetic_readmission_data$A1Cresult)

#Binarizing Variables - A1CRESULT

#A1Cresult
new_diabetic_readmission_data$A1Cresult <- as.factor(ifelse(new_diabetic_readmission_data$A1Cresult == ">7" |new_diabetic_readmission_data$A1Cresult == ">8" | new_diabetic_readmission_data$A1Cresult == "Norm",1,0))

#Deleting above 4 Medications 
new_diabetic_readmission_data <- new_diabetic_readmission_data[,!names(new_diabetic_readmission_data) %in% c('examide','citoglipton', 'glimepiride.pioglitazone', 'metformin.rosiglitazone', 'by')]

#acetohexamide
new_diabetic_readmission_data$acetohexamide <- as.factor(ifelse(new_diabetic_readmission_data$acetohexamide == "Down"| new_diabetic_readmission_data$acetohexamide == "Steady" | new_diabetic_readmission_data$acetohexamide == "Up",1,0))

#tolbutamide
new_diabetic_readmission_data$tolbutamide <- as.factor(ifelse(new_diabetic_readmission_data$tolbutamide == "Down"| new_diabetic_readmission_data$tolbutamide == "Steady" | new_diabetic_readmission_data$tolbutamide == "Up",1,0))

#troglitazone
new_diabetic_readmission_data$troglitazone <- as.factor(ifelse(new_diabetic_readmission_data$troglitazone == "Down"| new_diabetic_readmission_data$troglitazone == "Steady" | new_diabetic_readmission_data$troglitazone == "Up",1,0))

#tolazamide
new_diabetic_readmission_data$tolazamide <- as.factor(ifelse(new_diabetic_readmission_data$tolazamide == "Down"| new_diabetic_readmission_data$tolazamide == "Steady" | new_diabetic_readmission_data$tolazamide == "Up",1,0))

#glipizide.metformin
new_diabetic_readmission_data$glipizide.metformin <- as.factor(ifelse(new_diabetic_readmission_data$glipizide.metformin == "Down"| new_diabetic_readmission_data$glipizide.metformin == "Steady" | new_diabetic_readmission_data$glipizide.metformin == "Up",1,0))

#metformin.pioglitazone
new_diabetic_readmission_data$metformin.pioglitazone <- as.factor(ifelse(new_diabetic_readmission_data$metformin.pioglitazone == "Down"| new_diabetic_readmission_data$metformin.pioglitazone == "Steady" | new_diabetic_readmission_data$metformin.pioglitazone == "Up",1,0))

#Binarizing Variables - THREE LEVELS 

#acarbose
new_diabetic_readmission_data$acarbose <- as.factor(ifelse(new_diabetic_readmission_data$acarbose == "Down"| new_diabetic_readmission_data$acarbose == "Steady" | new_diabetic_readmission_data$acarbose == "Up",1,0))

#FOUR LEVELS (Total: 12)
#metformin, repaglinide, nateglinide, chlorpropamide, glimepiride, glipizide, glyburide, pioglitazone, rosiglitazone, miglitol, insulin, glyburide.metformin

#metformin
new_diabetic_readmission_data$metformin <- as.factor(ifelse(new_diabetic_readmission_data$metformin == "Down"| new_diabetic_readmission_data$metformin == "Steady" | new_diabetic_readmission_data$metformin == "Up",1,0))

#repaglinide
new_diabetic_readmission_data$repaglinide <- as.factor(ifelse(new_diabetic_readmission_data$repaglinide == "Down"| new_diabetic_readmission_data$repaglinide == "Steady" | new_diabetic_readmission_data$repaglinide == "Up",1,0))

#nateglinide
new_diabetic_readmission_data$nateglinide <- as.factor(ifelse(new_diabetic_readmission_data$nateglinide == "Down"| new_diabetic_readmission_data$nateglinide == "Steady" | new_diabetic_readmission_data$nateglinide == "Up",1,0))

#chlorpropamide
new_diabetic_readmission_data$chlorpropamide <- as.factor(ifelse(new_diabetic_readmission_data$chlorpropamide == "Down"| new_diabetic_readmission_data$chlorpropamide == "Steady" | new_diabetic_readmission_data$chlorpropamide == "Up",1,0))

#glimepiride
new_diabetic_readmission_data$glimepiride <- as.factor(ifelse(new_diabetic_readmission_data$glimepiride == "Down"| new_diabetic_readmission_data$glimepiride == "Steady" | new_diabetic_readmission_data$glimepiride == "Up",1,0))

#glipizide
new_diabetic_readmission_data$glipizide <- as.factor(ifelse(new_diabetic_readmission_data$glipizide == "Down"| new_diabetic_readmission_data$glipizide == "Steady" | new_diabetic_readmission_data$glipizide == "Up",1,0))

#glyburide
new_diabetic_readmission_data$glyburide <- as.factor(ifelse(new_diabetic_readmission_data$glyburide == "Down"| new_diabetic_readmission_data$glyburide == "Steady" | new_diabetic_readmission_data$glyburide == "Up",1,0))

#pioglitazone
new_diabetic_readmission_data$pioglitazone <- as.factor(ifelse(new_diabetic_readmission_data$pioglitazone == "Down"| new_diabetic_readmission_data$pioglitazone == "Steady" | new_diabetic_readmission_data$pioglitazone == "Up",1,0))

#rosiglitazone
new_diabetic_readmission_data$rosiglitazone <- as.factor(ifelse(new_diabetic_readmission_data$rosiglitazone == "Down"| new_diabetic_readmission_data$rosiglitazone == "Steady" | new_diabetic_readmission_data$rosiglitazone == "Up",1,0))

#miglitol
new_diabetic_readmission_data$miglitol <- as.factor(ifelse(new_diabetic_readmission_data$miglitol == "Down"| new_diabetic_readmission_data$miglitol == "Steady" | new_diabetic_readmission_data$miglitol == "Up",1,0))

#insulin
new_diabetic_readmission_data$insulin <- as.factor(ifelse(new_diabetic_readmission_data$insulin == "Down"| new_diabetic_readmission_data$insulin == "Steady" | new_diabetic_readmission_data$insulin == "Up",1,0))

#glyburide.metformin
new_diabetic_readmission_data$glyburide.metformin <- as.factor(ifelse(new_diabetic_readmission_data$glyburide.metformin == "Down"| new_diabetic_readmission_data$glyburide.metformin == "Steady" | new_diabetic_readmission_data$glyburide.metformin == "Up",1,0))


#change
new_diabetic_readmission_data$change <- as.factor(ifelse(new_diabetic_readmission_data$change == "Ch",1,0))

new_diabetic_readmission_data$diabetesMed <- as.factor(ifelse(new_diabetic_readmission_data$diabetesMed == "No",1,0))

### step 1 Model: 
new_diabetic_readmission_data$y <- as.factor(ifelse(new_diabetic_readmission_data$readmitted == '<30' | 
                                                      new_diabetic_readmission_data$readmitted == '>30', 1,0))

## step2 Model: 

print("Completed Binarizing the categorical variables")




