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
library(Matrix)
library(glmnet)

#Reading in train and test data
train <- read.csv("cleaned_train_data.csv", na.strings = c("","NA","<NA>"))
test <- read.csv("cleaned_test_data.csv", na.strings = c("","NA","<NA>"))



#Logistic Regression
#--------------------
fit <- glm(data=train, readmitted ~ .-patientID, family="binomial")

logLoss(fit)


#Evaluating model
train$pred<-as.numeric(fit$fitted.values>0.5)
model_eval(fit, train)


#Making predictions on test data
pred_prob <- predict(fit, test, type = "response")
test$predReadmit <- pred_prob


#Writing Submission file
submission <- select(test, c(patientID, predReadmit))
write.csv(submission, "../hm7-group11-submission.csv", row.names = FALSE)




#Penalized Logistic Regression
#-------------------------------
x = model.matrix(readmitted~.,data=train[,-1])
y = ifelse(train$readmitted == "pos",1,0)

x = model.matrix(readmitted~.,data=train[,-c(1)])
y = train[,30]

cv.lasso <- cv.glmnet(x,y,alpha=0,family="binomial")
print(cv.lasso$lambda.min)

fit <- glmnet(x,y,alpha=0,family="binomial",lambda=0.5)

plot(cv.lasso)
print(cv.lasso$lambda.min)

x.test <- model.matrix(~.,data=test)
predicted.classes = 0

probabilities <- fit %>% predict(newx=x.test)
predicted.classes <- ifelse(probabilities > 0.0001, 1, 0)

sum(predicted.classes)

x.test <- model.matrix(readmitted~., test)
fit = glmnet(x=train_[,2:29],y=train_[,30],family="binomial",alpha=0.2,nlambda=20)
plot(fit)
coef(fit,s=0.1)

cvfit = cv.glmnet(x=train_[,2:29],y=train_[,30])
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
cvfit$glmnet.fit



#Decision Tree
#---------------
split <- vector()
ll <- vector()
for (i in 10:30) {
  tuning <- rpart.control(minsplit = 2,
                          minbucket = 2,
                          maxdepth = i,
                          cp = 0)
  
  fit_dt <- rpart(readmitted~.-patientID, data = train, method = 'class', control = tuning)
  
  a <- as.data.frame(predict(fit_dt, type="prob")) 
  lloss <- logLoss(train$readmitted, as.numeric(unlist(a["1"])))
  
  split <- c(split, i)
  ll <- c(ll, lloss)
  
  print(lloss)
}


#Predicting test data
pred_prob_dt <-predict(fit_dt, test, type = 'prob')
test$predReadmit <- pred_prob_dt


#Evaluating model
train$pred<-predict(fit_dt, type = 'class')
model_eval(fit_dt, train)


submission_dt <- select(test, c(patientID, predReadmit))
write.csv(submission_dt, "../hm7-group11-submission_dt5.csv", row.names = FALSE)




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
fit_dt <- train(readmitted ~ .-patientID, 
                data = train,
                method = "rpart",
                trControl = caret.control,
                tuneLength = 15)
fit_dt

#Model
bestDT <- fit_dt$finalModel
bestDT

prp(bestDT, type = 0, extra = 1, under = TRUE)

pred_prob_dt <- predict(fit_dt, test, type="prob")
test$predReadmit <- pred_prob_dt


#model_eval()
submission_dt <- select(test, c(patientID, predReadmit))
write.csv(submission_dt, "../hm7-group11-submission_dt1.csv", row.names = FALSE)





#Random Forest
#--------------

#cross validation using grid search repeated 10 times
control <- trainControl(method = "cv", number = 10, search="grid")

set.seed(5103)

train$readmitted <- as.factor(train$readmitted)

#train model

for (i in 1:14) {
  fit_rf <- randomForest(readmitted ~ .-patientID, data = train, mtry=i, ntree = 300, 
                         maxnodes=15)
  
  ll <- logLoss(fit_rf)
  print(ll)
}

#predicting test set
pred_prob_rf <- predict(fit_rf, test, type="prob")
test$predReadmit <- pred_prob_rf


#Evaluating model
train$pred<-predict(fit_rf, type = 'class')
model_eval(fit_rf, train)


submission_dt <- select(test, c(patientID, predReadmit))
write.csv(submission_dt, "../hm7-group11-submission_rf.csv", row.names = FALSE)




#Support Vector Machine
#------------------------

fit_svm <- svm(readmitted ~ .-patientID, data = train, type="C-classification",
               kernel="linear", probability=TRUE)


pred_prob_svm <- predict(fit_svm, test, probability = TRUE)
test$predReadmit <- attr(pred_prob_svm, "probabilities")



submission_svm <- select(test, c(patientID, predReadmit))
write.csv(submission_svm, "../hm7-group11-submission_svm.csv", row.names = FALSE)


#Evaluating model
train$pred<-predict(fit_svm, probability = TRUE)
model_eval(fit_svm, train)




# Neural Networks
#---------------------
glimpse(train)
hosp_matrix = model.matrix(~., data =train)
test_matrix = model.matrix(~.,data=test)

hosp = train %>%
  select(-c('time_in_hospital', 'age','num_medications','number_outpatient','number_emergency', 'num_procedures','gender','diabetesMed'))

str(hosp)

hosp_matrix <- model.matrix(~.,data=hosp)
test <- test %>% 
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
str(train)

pred = neuralnet::compute(nn1, test_matrix[,-c(1,22)]) # To predict on test set
class_pred = round(pred$net.result)