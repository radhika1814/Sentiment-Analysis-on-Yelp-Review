library(nnet)
library(caret)
library(ROCR)
library(pROC)

file = read.csv("Yelp.csv")


file = subset(file, select = -c(X.1, X, business_id, name, categories, city, state, stars))

intrain <- createDataPartition(y = file$BusinessType, p =0.80, list = FALSE)
trainx <- file[intrain,]
testx <- file[-intrain,]

numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
fit2 <- train(BusinessType ~ . , data = trainx, method = 'nnet',maxit = 1000,preProcess = c('center', 'scale'), trControl = numFolds, tuneGrid=expand.grid(size=c(40),decay=c(0.1)))
resultsf <- predict(fit2, newdata=trainx)
conf1 <- confusionMatrix(resultsf, trainx$BusinessType) 
print(conf1)
resultsf1 <- predict(fit2, newdata=testx)
conf2 <- confusionMatrix(resultsf1, testx$BusinessType, positive = "Good")
print(conf2)
goodbad<- function(x){
  if (x=="Good"){
    return(1)
  }
  else{
    return(0)
  }
}
predd = prediction(sapply(resultsf1,goodbad), sapply(testx$BusinessType,goodbad))
perf = performance(predd,"tpr","fpr")
plot(perf,lwd=2,col="blue",main="ROC - Neural Network on Adult")
abline(a=0,b=1)
