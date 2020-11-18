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

#Reading in train and test data
train <- read.csv("../hm7-Train.csv", na.strings = c("","NA","<NA>"))
test <- read.csv("../hm7-Test.csv", na.strings = c("","NA","<NA>"))


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

