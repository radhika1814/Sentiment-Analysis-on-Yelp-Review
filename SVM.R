library(caret)
library(e1071)
#library(kernlab)
library(LiblineaR)
library(ggplot2)
library(pROC)
library(InformationValue)
set.seed(1643)
file = read.csv("Yelp.csv")


file = subset(file, select = -c(X.1, X, business_id, name, categories, city, state, stars))

intrain <- createDataPartition(y = file$BusinessType, p =0.80, list = FALSE)
training <- file[intrain,]
testing <- file[-intrain,]

training[["BusinessType"]] = factor(training[["BusinessType"]])

#svm_model <- svm(BusinessType~., data = training, kernel = "radial", cost = 2, gamma = 1)
#summary(svm_model)
set.seed(42)
tune = expand.grid(c = 2)
svm <- train(BusinessType~., file, preProcess = c("center", "scale"),
             method = "svmLinear2", tuneGride = tune,
             trControl = trainControl(method = "cv", number = 10))

pred <- predict(svm, testing)

testResultSVM <- data.frame(obs = testing$BusinessType, pred = pred)
SVM.summary<-defaultSummary(testResultSVM)
SVM.summary
confusionMatrix(pred, testing$BusinessType, positive = "Good")


#PredictionsWithProbs <- predict(svm, testing, type ="prob")

#auc <- auc()
#auc(testing$BusinessType, pred)
##################################################################################
library(ROCR)

# convert good to 1, bad to 0
goodbad<- function(x){
  if (x=="Good"){
    return(1)
  }
  else{
    return(0)
  }
}
test.converted<-sapply(testResultSVM$obs,goodbad) 
pred.converted<-sapply(testResultSVM$pred,goodbad)


# roc


scores <- prediction(pred.converted, test.converted)


perf <- performance(scores, "tpr", "fpr")

plot(perf,
     main="SVM ROC Curve",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
