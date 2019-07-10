setwd("C:/Users/sanya/Desktop/OR 568/Project")

library(caret)

file = read.csv("Yelp.csv")


file = subset(file, select = -c(X.1, X, business_id, name, categories, city, state, stars))

intrain <- createDataPartition(y = file$BusinessType, p =0.80, list = FALSE)
trainx <- file[intrain,]
testx <- file[-intrain,]

training.x<- trainx[,-18]
training.y <- trainx[,18]
testing.x<- testx[,-18]
testing.y  <- testx[,18]

training.y = as.factor(training.y)
testing.y = as.factor(testing.y)

########## KNN model ###########################

set.seed(400)

ctrl<- trainControl(method = "repeatedcv", number = 10, repeats = 5)

knnFit <- train(x = training.x , y = training.y,
                method = "knn",
                preProc = c("center", "scale"),
                tuneLength = 20,
                trControl = ctrl)

knnFit

####### KNN performance evaluation #############
 
knn.pred<- predict(knnFit, testing.x)

knn.results <- data.frame(obs = testing.y, pred = knn.pred)

knn.summary<-defaultSummary(knn.results)
knn.summary

confusionMatrix(knn.pred,testing.y, positive = "Good")

knn.varImp <-varImp(knnFit)

plot(knn.varImp)

##################### KNN ROC ######################

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
test.converted<-sapply(knn.results$obs,goodbad) 
pred.converted<-sapply(knn.results$pred,goodbad)

knn_scores <- prediction(pred.converted, test.converted)

knn_perf <- performance(knn_scores, "tpr", "fpr")

plot(knn_perf,
     main="KNN ROC Curve",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")

############## KNN AUC ###################################

knn_auc <- performance(knn_scores, "auc")
knn_auc2<- as.numeric(knn_auc@y.values) 

