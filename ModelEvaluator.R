#A model evaluation function for two-class classifiers
#Author - Rafia Bushra
#Date Created - 11/15/20

library(caret)
library(ROCR)
library(ModelMetrics)
library(car)
library(rpart)
library(rpart.plot)



model_eval <- function(fit, df){
  
  
  #####Confusion Matrix & Statistics#####
  print("#####Confusion Matrix & Statistics#####")
  cmat <- confusionMatrix(as.factor(df$pred), as.factor(df$targ), positive="1")
  print(cmat)
  #####End of Confusion Matrix  & Statistics#####
  
  
  
  #####ROC curve and AUROC#####
  print("#####ROC curve and AUROC#####")
  pred <- prediction(fit$fitted.values, fit$y)
  perf <- performance(pred,"tpr","fpr") 
  
  plot(perf,colorize=TRUE, print.cutoffs.at = c(0.25,0.5,0.75)); 
  abline(0, 1, col="red")  
  
  
  #AUROC
  auroc <- performance(pred, measure = "auc")
  auroc <- auroc@y.values[[1]]
  cat("Area Under ROC Curve: ", auroc, "\n")
  #####End of ROC curve and AUROC#####
  
  
  
  #####D Statistic#####
  predVals <-  data.frame(trueVal=df$targ, predClass=df$pred, predProb=fitted(fit))
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
  ll <- logLoss(df$targ, df$pred)
  cat("Log Loss: ", ll, "\n")
  #####End of Log Loss#####
  
  output <- list("cMat" = cmat, "auroc" = auroc, "D_Stat" = dStat, "F1_Score" = f1, "MCC" = mcc, "LogLoss" = ll)
  return(output)
}
