library(tidyverse)
library(dplyr)


#Reading in data
train <- read.csv("../hm7-Train.csv")
test <- read.csv("../hm7-Test.csv")


#Changing other values to NAs
train <- train %>% replace_with_na(replace = list(max_glu_serum="None", A1Cresult="None"))
test <- test %>% replace_with_na(replace = list(max_glu_serum="None", A1Cresult="None"))


# Changing values that equal NA to "Unknown" for race
train <- train %>% mutate_at(vars(race),~replace(.,is.na(.),"Unknown"))
test <- test %>% mutate_at(vars(race),~replace(.,is.na(.),"Unknown"))


#Convert character vars to factors
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], 
                                             as.factor)
test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)], 
                                           as.factor)


#Convert some int columns with nominal values to factors
train <- train %>% mutate_at(vars(admission_type,discharge_disposition,
                                  admission_source),as.factor)
test <- test %>% mutate_at(vars(admission_type,discharge_disposition,
                                admission_source),as.factor)


#Convert patientID to char val
train <- train %>% mutate_at(vars(ends_with("ID")), as.character)
test <- test %>% mutate_at(vars(ends_with("ID")), as.character)


#Droping a few columns with high MV percentage 
train <- select(train, -c(payer_code, medical_specialty, max_glu_serum, A1Cresult))
test <- select(test, -c(payer_code, medical_specialty, max_glu_serum, A1Cresult))


#Remove columns that only have one-level factors
train <- select(train,-c(examide,citoglipton,glimepiride.pioglitazone))
test <- select(test,-c(examide,citoglipton,glimepiride.pioglitazone))


#Remove 2-drug combo columns
train <- select(train,-c(glyburide.metformin,glipizide.metformin,
                         metformin.rosiglitazone,metformin.pioglitazone))
test <- select(test,-c(glyburide.metformin,glipizide.metformin,
                       metformin.rosiglitazone,metformin.pioglitazone))


#Remove drugs that only have < 2 "non-No" entries
train <- select(train,-c(acetohexamide,troglitazone))
test <- select(test,-c(acetohexamide,troglitazone))


#Remove columns with unmatching levels between train and test
train <-select(train,-c(chlorpropamide, diagnosis))
test <-select(test,-c(chlorpropamide, diagnosis))


#Drop NA values
train <-drop_na(train)



#Write cleaned files
write.csv(train, "../cleaned_train_data.csv", row.names = FALSE)
write.csv(test, "../cleaned_test_data.csv", row.names = FALSE)
