#Predicting Hospital readmissions using different models
#Author - Rafia Bushra
#Date Created - 11/15/20

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
library(MLmetrics)
library(dplyr)
library(ROCR)
library(car)
library(rpart.plot)
library(ggplot2)
library(naniar)
library(dplyr)
library(mice)
library(VIM)


# instantiate evaluation function
model_eval <- function(fit, df){
  
  
  #####Confusion Matrix & Statistics#####
  print("#####Confusion Matrix & Statistics#####")
  cmat <- caret::confusionMatrix(as.factor(df$pred), as.factor(df$readmitted), positive="1")
  print(cmat)
  #####End of Confusion Matrix  & Statistics#####
  
  
  
  #####Kappa#####
  kp <- kappa(train$readmitted, train$pred)
  cat("Kappa: ", kp, "\n")
  #####End of Kappa#####
  
  
  
  
  #####ROC curve and AUROC#####
  # print("#####ROC curve and AUROC#####")
  #  pred <- prediction(fit$fitted.values, fit$y)
  #  perf <- performance(pred,"tpr","fpr") 
  
  #  plot(perf,colorize=TRUE, print.cutoffs.at = c(0.25,0.5,0.75)); 
  #  abline(0, 1, col="red")  
  
  
  #AUROC
  #  auroc <- performance(pred, measure = "auc")
  #  auroc <- auroc@y.values[[1]]
  #  cat("Area Under ROC Curve: ", auroc, "\n")
  #####End of ROC curve and AUROC#####
  
  
  
  #####D Statistic#####
  predVals <-  data.frame(trueVal=df$readmitted, predClass=df$pred, predProb=fitted(fit))
  df.1<-predVals[predVals$trueVal==1,]
  df.0<-predVals[predVals$trueVal==0,]
  
  #printing d statistic
  dStat <- mean(df.1$predProb) - mean(df.0$predProb)
  cat("D Statistic: ", dStat, "\n")
  #####End of D Statistic#####
  
  
  
  #####F1 Score#####
  f1 <- cmat$byClass["F1"]
  cat("F1 Score: ", f1, "\n")
  #####End of F1 Score#####
  
  
  
  #####Matthews correlation coefficient#####
  TP <- cmat$table[1,1]
  TN <- cmat$table[2,2]
  FP <- cmat$table[1,2]
  FN <- cmat$table[2,1]
  
  num <- (TP*TN - FP*FN)
  den <- 
    as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))
  
  mcc <- num/sqrt(den)
  cat("Matthews correlation coefficient: ", mcc, "\n")
  
  #####End of Matthews correlation coefficient#####
  
  
  
  #####Log Loss#####
  ll <- LogLoss(df$readmitted, df$pred)
  cat("Log Loss: ", ll, "\n")
  #####End of Log Loss#####
  
  output <- list("cMat" = cmat, "kappa" = kp, "D_Stat" = dStat, "F1_Score" = f1, "MCC" = mcc, "LogLoss" = ll)
  return(output)
}

#Clean Data
#-------------
#Reading in data
train <- read.csv("hm7-Train.csv")
test <- read.csv("hm7-Test.csv")


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
#------------------------------------------------------------------------


#Write cleaned files
write.csv(train, "cleaned_train_data.csv", row.names = FALSE)
write.csv(test, "cleaned_test_data.csv", row.names = FALSE)

#Reading in train and test data
train <- read.csv("cleaned_train_data.csv", na.strings = c("","NA","<NA>"))
test <- read.csv("cleaned_test_data.csv", na.strings = c("","NA","<NA>"))

#Logistic Regression
#--------------------
fit <- glm(data=train, readmitted ~ .-patientID, family="binomial")

LogLoss(fit)


#Evaluating model
train$pred<-as.numeric(fit$fitted.values>0.5)
model_eval(fit, train)


#Making predictions on test data
pred_prob <- predict(fit, test, type = "response")
test$predReadmit <- pred_prob

#Writing Submission file
submission <- select(test, c(patientID, predReadmit))
write.csv(submission, "../hm7-group11-submission.csv", row.names = FALSE)

#Metrics for Logistic Regression
#----------------------------
print(summary(fit))
print(exp(coef(fit)))

#calculate residuals
pearsonRes <-residuals(fit,type="pearson")
devianceRes <-residuals(fit,type="deviance")
rawRes <-residuals(fit,fit="response")
studentDevRes<-rstudent(fit)
fv<-fitted(fit)

train$pred <- as.numeric(fit$fitted.values>0.5)
predVals <- data.frame(trueVal=train$readmitted, predClass=train$pred, predProb=fv, 
                       rawRes, pearsonRes, devianceRes, studentDevRes)
tail(predVals)


plot(studentDevRes,main="studentDevRes")
barplot(studentDevRes,main="studentDevRes")

plot(predict(fit),residuals(fit),main="Predicted Values vs Residuals")  #plot predicted value vs residuals
abline(h=0,lty=2,col="grey")

plot(predict(fit),residuals(fit),main="Predicted Values vs Residuals",col=c("blue","red")[1+train$readmitted])
abline(h=0,lty=2,col="grey")

barplot(cooks.distance(fit),main="Cooks Distance")
influence.measures(fit)
influencePlot(fit,main="Influence Plot")

print(vif(fit))


#Penalized Logistic Regression
#-------------------------------
train$readmitted = as.factor(train$readmitted)
trctrl <- trainControl(method = "cv",number=10)
enetFit <- train(readmitted~., data = train,
                 method = "glmnet",
                 trControl=trctrl,
                 tuneGrid = data.frame(alpha=1,
                                       lambda=seq(0.0,0.7,0.05)))

enetFit$bestTune

train$pred=predict(enetFit, train)


model_eval(enetFit, train)
enetFit$bestTune
test$predReadmit = predict(enetFit,test)

submission_plrm <- select(test, c(patientID, predReadmit))
write.csv(submission_plrm, "../hm7-group11-submission_plrm.csv", row.names = FALSE)


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
# you can ignore this, it did not prove fruitful 
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

nn1$generalized.weights # To check the weights
test = hosp_matrix_[,-c(1,22)]
str(train)

pred = neuralnet::compute(nn1, test_matrix[,-c(1,22)]) # To predict on test set
class_pred = round(pred$net.result)