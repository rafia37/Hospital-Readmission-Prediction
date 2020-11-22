library(Matrix)
library(glmnet)
library(caret)
library(car)
library(ggplot2)
library(gmodels)
library(car)
library(ROCR)
library(rpart)          # for decision tree modeling
library(party)          # for visualizing trees
library(partykit)       # for visualizing trees
library(rpart.plot)			# enhanced tree plots
library(RColorBrewer)		# color selection for fancy tree plot


heart <- read.csv("heartFailure.csv")


modelThis <-function(df,target,formula){
  # create logistic regression model 
  logFit <- glm(data=df,eval(substitute(formula)),family="binomial")
  print(summary(logFit))
  print(exp(coef(logFit)))
  print(exp(confint(logFit)))
  
  #calculate residuals
  pearsonRes <-residuals(logFit,type="pearson")
  devianceRes <-residuals(logFit,type="deviance")
  rawRes <-residuals(logFit,type="response")
  studentDevRes<-rstudent(logFit)
  fv<-fitted(logFit)
  
  df$pred <- as.numeric(logFit$fitted.values>0.38)
  predVals <- data.frame(trueVal=eval(substitute(target)), predClass=df$pred, predProb=fv, 
                         rawRes, pearsonRes, devianceRes, studentDevRes)
  tail(predVals)
  plot(studentDevRes,main="studentDevRes")
  barplot(studentDevRes,main="studentDevRes")
  
  plot(predict(logFit),residuals(logFit),main="Predicted Values vs Residuals")  #plot predicted value vs residuals
  #abline(h=0,lty=2,col="grey")
  
  #plot(predict(fit),residuals(fit),col=c("blue","red")[1+substitute(target)])
  #abline(h=0,lty=2,col="grey")
  
  rl=loess(residuals(logFit)~predict(logFit))
  X<-data.frame(yHat=rl$x,predictedR=rl$fitted)
  X<-X[order(X[,1]),]
  lines(X,col="black",lwd=1)
  
  rl=loess(residuals(logFit)~predict(logFit))
  y=predict(rl,se=TRUE)
  segments(predict(logFit),y$fit+2*y$se.fit,predict(logFit),y$fit-2*y$se.fit,col="green")
  
  barplot(cooks.distance(logFit),main="Cooks Distance")
  
  influence.measures(logFit)
  influencePlot(logFit,main="Influence Plot")
  
  # variance inflation
  print(vif(logFit))
  print(CrossTable(df$pred))
  
  pred <- prediction(logFit$fitted.values, logFit$y)    #ROC curve for training data
  perf <- performance(pred,"tpr","fpr") 
  
  pred <- prediction(logFit$fitted.values, logFit$y)    #ROC curve for training data
  perf <- performance(pred,"tpr","fpr") 
  
  plot(perf,colorize=TRUE, print.cutoffs.at = c(0.25,0.5,0.75)); 
  abline(0, 1, col="red")  
  
  perf <- performance(pred, "acc")
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))
  
  # can also look at cutoff based on different cost structure
  perf <- performance(pred, "cost", cost.fp=1, cost.fn=5)
  plot(perf) 
  
  # building decision tree
  #df=data.frame(df)
  #fitDT<-rpart(formula,data=df,                   
  #             parms=list(split="information"),   #can change to information gain
  #             control=rpart.control(xval=20)  )  #can change k-fold CV   
              
  #plot(fitDT)

  #fitDT<-rpart(formula,data=ds,control=rpart.control(cp=0.001),xval=20)   
  #prp(fitDT)
  #printcp(fitDT)
  #plotcp(fitDT)
  
  
  #table(df$predicted)
}

heart=data.frame(heart)
modelThis(heart,"heart$death","death ~ serum_creatinine*ejection_fraction + .")

