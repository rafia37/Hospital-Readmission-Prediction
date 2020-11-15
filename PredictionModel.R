#Predicting Hospital readmissions using different models
#Author - Rafia Bushra
#Date Created - 11/15/20

source("ModelEvaluator.R")
library(tidyverse)

#Reading in train and test data
train <- read.csv("../hm7-Train.csv", na.strings = c("","NA","<NA>"))
test <- read.csv("../hm7-Test.csv", na.strings = c("","NA","<NA>"))


#Choosing only predictor columns and target
new_train <- select(train, -c(race, payer_code, medical_specialty, diagnosis, max_glu_serum:metformin.pioglitazone))
new_test <- select(test, -c(race, payer_code, medical_specialty, diagnosis, max_glu_serum:metformin.pioglitazone))


#Fitting logistic regression model
#gender+age+admission_type+discharge_disposition+admission_source+time_in_hospital+num_lab_procedures+num_procedures+num_medications+number_outpatient+number_emergency+number_inpatient+number_diagnoses+diabetesMed
fit <- glm(data=new_train, readmitted ~ .-patientID, family="binomial")

#Taking a look at predictions and fitting
summary(fit)
exp(coef(fit))

new_train$pred<-as.numeric(fit$fitted.values>0.5)
#names(train)[names(train)=="readmitted"] <- "targ"

#train$pred <- as.factor(train$pred)
#train$targ <- as.factor(train$targ)

logLoss(new_train$readmitted, new_train$pred)
logLoss(fit)

#Making predictions on test data
pred_prob <- predict(fit, test, type = "response")
new_test$predReadmit <- pred_prob


#model_eval()
submission <- select(new_test, c(patientID, predReadmit))
write.csv(submission, "../hm7-group11-submission.csv", row.names = FALSE)
