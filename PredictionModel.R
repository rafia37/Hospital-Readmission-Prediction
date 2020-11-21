#Predicting Hospital readmissions using different models
#Author - Rafia Bushra
#Date Created - 11/15/20

source("ModelEvaluator.R")
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(ModelMetrics)
library(neuralnet) # Used for neuralnet function
library(readr) # To read csv files
library(caTools) # To split data
library(Metrics) # To calculate RMSE value

#Reading in train and test data
train <- read.csv("hm7-Train.csv", na.strings = c("","NA","<NA>"))
test <- read.csv("hm7-Test.csv", na.strings = c("","NA","<NA>"))


#Choosing only predictor columns and target
new_train <- select(train, -c(race, diagnosis, payer_code, medical_specialty, max_glu_serum:metformin.pioglitazone))
new_test <- select(test, -c(race, diagnosis, payer_code, medical_specialty, max_glu_serum:metformin.pioglitazone))
new_train$readmitted <- as.factor(new_train$readmitted)
#new_train <- drop_na(new_train)



#Logistic Regression
#--------------------
fit <- glm(data=new_train, readmitted ~ .-patientID, family="binomial")

logLoss(fit)


#Evaluating model
new_train$pred<-as.numeric(fit$fitted.values>0.5)
#names(new_train)[names(new_train)=="readmitted"] <- "targ"
#new_train$pred <- as.factor(new_train$pred)
#new_train$targ <- as.factor(new_train$targ)
model_eval(fit, new_train, "readmitted")


#Making predictions on test data
pred_prob <- predict(fit, new_test, type = "response")
new_test$predReadmit <- pred_prob


#model_eval()
submission <- select(new_test, c(patientID, predReadmit))
write.csv(submission, "../hm7-group11-submission.csv", row.names = FALSE)





#Decision Tree
#---------------
split <- vector()
ll <- vector()
for (i in 1:10) {
  tuning <- rpart.control(minsplit = 3,
                          minbucket = 5,
                          maxdepth = i,
                          cp = 0)
  
  fit_dt <- rpart(readmitted~.-patientID, data = new_train, method = 'class', control = tuning)
  
  a <- as.data.frame(predict(fit_dt, type="prob")) 
  lloss <- logLoss(new_train$readmitted, as.numeric(unlist(a["1"])))
  
  split <- c(split, i)
  ll <- c(ll, lloss)
  
  print(lloss)
}

pred_prob_dt <-predict(fit_dt, new_test, type = 'prob')
new_test$predReadmit <- pred_prob_dt


#model_eval()
submission_dt <- select(new_test, c(patientID, predReadmit))
write.csv(submission_dt, "../hm7-group11-submission_dt4.csv", row.names = FALSE)




#Decision Tree with cross-validation
#-----------------------------------
# Setting a seed for reproducability
set.seed(5013)


# 10-fold cross validation repeated 3 times
caret.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 3)

new_train$readmitted <- as.factor(new_train$readmitted)

#using 15 values for tuning the cp parameter for rpart. 
fit_dt <- train(readmitted ~ .-patientID, 
                data = new_train,
                method = "rpart",
                trControl = caret.control,
                tuneLength = 15)
fit_dt

#Model
bestDT <- fit_dt$finalModel
bestDT

prp(bestDT, type = 0, extra = 1, under = TRUE)

pred_prob_dt <- predict(fit_dt, new_test, type="prob")
new_test$predReadmit <- pred_prob_dt


#model_eval()
submission_dt <- select(new_test, c(patientID, predReadmit))
write.csv(submission_dt, "../hm7-group11-submission_dt1.csv", row.names = FALSE)





#Random Forest
#--------------

#cross validation using grid search repeated 10 times
control <- trainControl(method = "cv", number = 10, search="grid")

set.seed(5103)

#train model

for (i in 10:30) {
  fit_rf <- randomForest(readmitted ~ .-patientID, data = new_train, mtry=2, ntree = 50, 
                         maxnodes=i)
  
  ll <- logLoss(fit_rf)
  print(ll)
}

pred_prob_rf <- predict(fit_rf, new_test, type="prob")
new_test$predReadmit <- pred_prob_rf


#model_eval()
submission_dt <- select(new_test, c(patientID, predReadmit))
write.csv(submission_dt, "../hm7-group11-submission_rf.csv", row.names = FALSE)




#Support Vector Machine
#------------------------

fit_svm <- svm(readmitted ~ .-patientID, data = new_train, type="C-classification",
               kernel="polynomial", probability=TRUE)

a <- predict(fit_svm, probability = TRUE)

pred_prob_svm <- predict(fit_svm, new_test, probability = TRUE)
new_test$predReadmit <- attr(pred_prob_svm, "probabilities")


#model_eval()
submission_svm <- select(new_test, c(patientID, predReadmit))
write.csv(submission_svm, "../hm7-group11-submission_svm_poly.csv", row.names = FALSE)

# Neural Networks
#---------------------
glimpse(new_train)
hosp_matrix = model.matrix(~., data =new_train)
test_matrix = model.matrix(~.,data=new_test)

hosp = new_train %>%
  select(-c('time_in_hospital', 'age','num_medications','number_outpatient','number_emergency', 'num_procedures','gender','diabetesMed'))

str(hosp)

hosp_matrix <- model.matrix(~.,data=hosp)
test <- new_test %>% 
  select(-c('predReadmit','time_in_hospital', 'age','num_medications','number_outpatient','number_emergency', 'num_procedures','gender','diabetesMed'))
empty <- test$patientID*0
test <- cbind(test,empty)
length(test)

test_matrix < as.data.frame(test)


# Ignore the following for now
colnames(hosp_matrix)[4] = "genderUnknownOrInvalid"
colnames(hosp_matrix)[5] = "tenToTwenty"
colnames(hosp_matrix)[6] = "twentyToThirty"
colnames(hosp_matrix)[7] = "thirtyToForty"
colnames(hosp_matrix)[8] = "fortyToFifty"
colnames(hosp_matrix)[9] = "fiftyToSixty"
colnames(hosp_matrix)[10] = "sixtyToSeventy"
colnames(hosp_matrix)[11] = "seventyToEighty"
colnames(hosp_matrix)[12] = "eightyToNinety"
colnames(hosp_matrix)[13] = "ninetyToHundred"
# ignore above for now


fmla <- formula(paste("readmitted1 ~ ", paste(colnames(hosp_matrix[,-c(1,22)]), collapse= "+"), collapse = ""))

nn1 = neuralnet(fmla, data = hosp_matrix,
                hidden = 1,
                algorithm = 'backprop',
                learningrate = 0.0001,
                err.fct="ce",
                linear.output = FALSE,
                stepmax = 1e+06)


plot(nn1) # Plotting the neuralnet

nn1$result.matrix

nn1$generalized.weights # To check the weights
test = hosp_matrix_[,-c(1,22)]
str(new_train)

pred = neuralnet::compute(nn1, test_matrix[,-c(1,22)]) # To predict on test set
class_pred = round(pred$net.result)
