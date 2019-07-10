#************************************RANDOM FOREST*******************************
#install.packages("lubridate")
setwd("C:/Mason/SYST 568 - Applied Predictive/Project Proposal/data/drive-download-20180306T173924Z-001")

Yelp_US_Restaurants_final <- read.csv("Yelp.csv", stringsAsFactors=FALSE)
library(AppliedPredictiveModeling)
library(caret)
library(randomForest)
library(lubridate)

############################# Remove unncessary columns ###############################
data<-Yelp_US_Restaurants_final[,-c(1,2,3,4,5,6,7,8)]


####################################### Add new column #################################
Yelp_US_Restaurants_final$BusinessType[Yelp_US_Restaurants_final$stars>=3] <- "Good"
Yelp_US_Restaurants_final$BusinessType[Yelp_US_Restaurants_final$stars<3] <- "Bad"


############################# Partition Data ###########################################
set.seed(1) 
n <- nrow(data)  
t <- 0.8*n            # 80% Trainig and 20% Testing
trainIndex <- sample(1:n,t)

training.x<- data[trainIndex,]
training.y<- data[trainIndex,18]

testing.x<- data[-trainIndex,]
testing.y<- data[-trainIndex,18]



############################# Random Forest ###########################################
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(7)
mtry <- sqrt(ncol(training.x))
tunegrid <- expand.grid(.mtry=mtry)

rfModel <- randomForest(as.factor(training.y) ~ WheelchairAccessible + OutdoorSeating  + review_count + GoodForMeal_lunch + GoodForMeal_brunch + GoodForMeal_breakfast + GoodForMeal_dinner + GoodForMeal_latenight
                        +  CoatCheck + BikeParking + Caters + BusinessParking_valet +  BusinessParking_validated +BusinessParking_street + BusinessParking_lot + BusinessParking_valet +DogsAllowed + NumberOfCheckins,
                        data=training.x,
                        importance=TRUE, tuneGrid=tunegrid, trControl=control,
                        ntree=500)

print(rfModel)
rf_yHat = predict(rfModel,newdata=testing.x)
## performance evaluation
rfPR = postResample(pred=rf_yHat, obs=as.factor(testing.y))
rfPR

testResultsRF <- data.frame(obs = testing.y, pred = rf_yHat)

confusionMatrix(rf_yHat,testing.y)

dev.off()
dev.new()
varImpPlot(rfModel)




######################################### Changing Tuning Parameters ###############################
############# ntree=2000############
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(seed)
tunegrid <- expand.grid(.mtry=mtry)

rfModel <- randomForest(as.factor(training.y) ~ WheelchairAccessible + OutdoorSeating  + review_count + GoodForMeal_lunch + GoodForMeal_brunch + GoodForMeal_breakfast + GoodForMeal_dinner + GoodForMeal_latenight
                        +  CoatCheck + BikeParking + Caters + BusinessParking_valet +  BusinessParking_validated +BusinessParking_street + BusinessParking_lot + BusinessParking_valet +DogsAllowed + NumberOfCheckins,
                        data=training.x,
                        importance=TRUE, tuneGrid=tunegrid, trControl=control, ntree=2000
                        )

print(rfModel)
rf_yHat = predict(rfModel,newdata=testing.x)
## performance evaluation
rfPR = postResample(pred=rf_yHat, obs=as.factor(testing.y))
rfPR

testResultsRF <- data.frame(obs = testing.y, pred = rf_yHat)
confusionMatrix(rf_yHat,testing.y, positive ="Good")

dev.off()
dev.new()
varImpPlot(rfModel)



######################################### Changing Tuning Parameters ###############################
############# ntree=2000############
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(seed)
tunegrid <- expand.grid(.mtry=mtry)

rfModel <- randomForest(as.factor(training.y) ~ WheelchairAccessible + OutdoorSeating  + review_count + GoodForMeal_lunch + GoodForMeal_brunch + GoodForMeal_breakfast + GoodForMeal_dinner + GoodForMeal_latenight
                        +  CoatCheck + BikeParking + Caters + BusinessParking_valet +  BusinessParking_validated +BusinessParking_street + BusinessParking_lot + BusinessParking_valet +DogsAllowed + NumberOfCheckins,
                        data=training.x,
                        importance=TRUE, tuneGrid=tunegrid, trControl=control, ntree=8000
)

print(rfModel)
rf_yHat = predict(rfModel,newdata=testing.x)
## performance evaluation
rfPR = postResample(pred=rf_yHat, obs=as.factor(testing.y))
rfPR

testResultsRF <- data.frame(obs = testing.y, pred = rf_yHat)
confusionMatrix(rf_yHat,testing.y)

dev.off()
dev.new()
varImpPlot(rfModel)


########################################### ROC Curve ###################################################
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
test.converted<-sapply(testResultsRF$obs,goodbad) 
pred.converted<-sapply(testResultsRF$pred,goodbad)


# roc
RF_scores <- prediction(pred.converted, test.converted)
RF_perf <- performance(RF_scores, "tpr", "fpr")

plot(RF_perf,
     main="Random Forest ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")

#training.y <- as.factor(training.y)
#training.x <- as.data.frame(unclass(training.x))

#str(training.x)
#*************************************SVM******************************************
# library(caret)
# library(e1071)
# #library(kernlab)
# library(LiblineaR)
# library(ggplot2)
# library(pROC)
# library(InformationValue)
# 
# 
# 
# tune = expand.grid(c = 2)
# svm <- train(BusinessType~., file, preProcess = c("center", "scale"),
#              method = "svmLinear2", tuneGride = tune,
#              trControl = trainControl(method = "cv", number = 10))
# 
# pred <- predict(svm, testing)
# 
# testResultSVM <- data.frame(obs = testing$BusinessType, pred = pred)
# SVM.summary<-defaultSummary(testResultSVM)
# SVM.summary
# confusionMatrix(pred, testing$BusinessType)
# 
# 
# #PredictionsWithProbs <- predict(svm, testing, type ="prob")
# 
# #auc <- auc()
# #auc(testing$BusinessType, pred)
# ##################################################################################
# library(ROCR)
# 
# # convert good to 1, bad to 0
# goodbad<- function(x){
#   if (x=="Good"){
#     return(1)
#   }
#   else{
#     return(0)
#   }
# }
# test.converted<-sapply(testResultSVM$obs,goodbad) 
# pred.converted<-sapply(testResultSVM$pred,goodbad)
# 
# 
# # roc
# 
# 
# scores <- prediction(pred.converted, test.converted)
# 
# 
# perf <- performance(scores, "tpr", "fpr")
# 
# plot(perf,
#      main="SVM ROC Curve",
#      xlab="1 - Specificity: False Positive Rate",
#      ylab="Sensitivity: True Positive Rate",
#      col="darkblue",  lwd = 3)
# abline(0,1, lty = 300, col = "green",  lwd = 3)
# grid(col="aquamarine")
