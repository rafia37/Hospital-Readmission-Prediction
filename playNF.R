#Hospital Readmission - Initial exploration of the data
#Author - Rafia Bushra
#Date Created - 11/15/20
library(car)
library(caret)
library(dplyr)
library(earth)
library(ggplot2)
library(mice)
library(ModelMetrics)
library(naniar)
library(rgl)
library(ROCR)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(VIM)
source("ModelEvaluator.R")

#Reading in train data
train <- read.csv("hm7-Train.csv",na.strings=c("","NA","<NA>"))
test  <- read.csv("hm7-Test.csv",na.strings= c("","NA","<NA>"))

#Initial ideas/observation based on glimpse
#--------------------------------
#Separate numeric and factor columns
#Turn binary variables to 0/1 perhaps (?) - Gender, diabetesMed
#Change the age range to a numeric column where we only keep the upper limit of the range (maybe)
#Make histograms to see distribution - race, gender, admission type,discharge, admission source, payer_code
#Make Histograms to see skew - time, #labs, #proc, #medication, #outpatient, #emergency, #inpatient, #diagnoses
#Questionable attributes - Admission Source, medical specialty, #proc, max_glu_serum, A1Cresult, Medication Columns (metformin:metformin.piog)
#Lots of missing - payer_code, max_glu_serum, A1Cresult, medical specialty
#Change Nones to NAs - max_glu_serum, A1Cresult

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

#Observation
#------------
#Droping a few columns with high MV percentage 
#shoots up the complete cases percentage from 28% to 98% 
train <- select(train, -c(payer_code, medical_specialty, max_glu_serum, A1Cresult))
test <- select(test, -c(payer_code, medical_specialty, max_glu_serum, A1Cresult))


#Observation
#------------
#glimepiride.pioglitazone, citoglipton, examide, has only one value "no". 
#Which means these 3 are useless as predictors 
#Remove columns that only have one-level factors
train <- select(train,-c(examide,citoglipton,glimepiride.pioglitazone))
test <- select(test,-c(examide,citoglipton,glimepiride.pioglitazone))
#Remove 2-drug combo columns
train <- select(train,-c(glyburide.metformin,glipizide.metformin,
                         metformin.rosiglitazone,metformin.pioglitazone))

#Remove drugs that only have < 2 "non-No" entries
train <- select(train,-c(acetohexamide,troglitazone))

#Remove diagnosis
train <-select(train,-c(diagnosis))
test <-select(test,-c(diagnosis))

#Drop NA values
train <-drop_na(train)

#### MODEL TIME!!! ####
#Fitting logistic regression model (Best = 0.6418941)
fit <- glm(data=train, readmitted ~ race+gender+age+admission_type+
             discharge_disposition+admission_source+time_in_hospital+
             num_lab_procedures+num_procedures+num_medications+number_outpatient+
             number_emergency+number_inpatient+number_diagnoses+diabetesMed, family="binomial")

fit <- glm(data=train, readmitted ~ race+gender+age+admission_type+
             discharge_disposition+admission_source+time_in_hospital+
             num_lab_procedures+num_procedures+num_medications+number_outpatient+
             number_emergency+number_inpatient+number_diagnoses+metformin+repaglinide
             +nateglinide+glimepiride+glipizide+glyburide+pioglitazone+rosiglitazone+
             acarbose+miglitol+insulin+diabetesMed, family="binomial")

fit <- glm(data=train,readmitted~.-patientID-diagnosis,family="binomial")

#Fitting MARS model
marsFit <- earth(readmitted ~ race+gender+age+admission_type+
                   discharge_disposition+admission_source+time_in_hospital+
                   num_lab_procedures+num_procedures+num_medications+number_outpatient+
                   number_emergency+number_inpatient+number_diagnoses+diabetesMed,data=train)
summary(marsFit)
#Taking a look at predictions and fitting
summary(fit)
exp(coef(fit))


train$pred<-as.numeric(fit$fitted.values>0.50)
logLoss(fit)


train$Mpred<-as.numeric(marsFit$fitted.values>0.50)
logLoss(train$readmitted,train$Mpred)

#Making predictions on test data
pred_prob <- predict(fit,test,type="response")
test$predReadmit <- pred_prob
sum(is.na(pred_prob))
#model_eval()
submission <- select(test, c(patientID, predReadmit))
write.csv(submission, "hm7-group11-submission3nf.csv", row.names = FALSE)






#Decision Tree with cross-validation
#-----------------------------------
# Setting a seed for reproducability
set.seed(5013)


# 10-fold cross validation repeated 3 times
caret.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 3)

train$readmitted <- as.factor(train$readmitted)

#using 15 values for tuning the cp parameter for rpart. 
fit_dt <- train(readmitted ~ race+gender+age+admission_type+discharge_disposition+
                  admission_source+time_in_hospital+num_lab_procedures+
                  num_procedures+num_medications+number_outpatient+number_emergency+
                  number_inpatient+number_diagnoses+metformin+repaglinide+nateglinide+
                  glimepiride+glipizide+glyburide+pioglitazone+rosiglitazone+acarbose+
                  miglitol+insulin+diabetesMed,data = train,method = "rpart",
                trControl = caret.control,tuneLength = 15)
fit_dt

#Model
bestDT <- fit_dt$finalModel
bestDT

prp(bestDT, type = 0, extra = 1, under = TRUE)

pred_prob_dt <- predict(fit_dt,test,type="prob")
test$predReadmit <- pred_prob_dt


#model_eval()
submission_dt <- select(test, c(patientID, predReadmit))
write.csv(submission_dt, "../hm7-group11-submission_dt1nf.csv", row.names = FALSE)





