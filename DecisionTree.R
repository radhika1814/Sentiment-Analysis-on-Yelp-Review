setwd("~/Desktop/Data Science (Google)/Data Analytics MS/Classes/SYST 568: Applied Predictive Analytics  /Course Projects and Exams/Project")
library(rpart)				        # Popular decision tree algorithm
library(rpart.plot)			    	# Enhanced tree plots
library(RColorBrewer)			  	# Color selection for fancy tree plot
library(party)					      # Alternative decision tree algorithm
library(partykit)				      # Convert rpart object to BinaryTree
library(caret)					      # Just a data source for this script
library(e1071)
library(tree)
library(data.table)

############################# Load Data ############################

# load
file = read.csv("Yelp.csv")


file = subset(file, select = -c(X.1, X, business_id, name, categories, city, state, stars))

intrain <- createDataPartition(y = file$BusinessType, p =0.80, list = FALSE)
trainx <- file[intrain,]
testx <- file[-intrain,]

trainx[["BusinessType"]] = factor(trainx[["BusinessType"]])

training.x<- trainx[,-18]
training.y <- trainx[,18]
testing.x<- testx[,-18]
testing.y  <- testx[,18]

training.y = as.factor(training.y)
testing.y = as.factor(testing.y)

############################# Tree Model/ ROC/ AUC ###########################

train_control<- trainControl(method="cv", number=10)

# train the model 

clasmodel<- train(x=training.x, y=training.y, trControl=train_control, method="rpart")
tree.pred<- predict(clasmodel, testing.x)
testResultsTREE <- data.frame(obs = testing.y, pred = tree.pred)
tree.summary<-defaultSummary(testResultsTREE)
tree.summary
cm.tree2<-confusionMatrix(tree.pred,testing.y, positive = "Good")

# ROC

# convert good to 1, bad to 0
goodbad<- function(x){
  if (x=="Good"){
    return(1)
  }
  else{
    return(0)
  }
}
test.converted<-sapply(testResultsTREE$obs,goodbad) 
pred.converted<-sapply(testResultsTREE$pred,goodbad)

library(ROCR)

tree_scores <- prediction(pred.converted, test.converted)

#ROC Curve
tree_perf <- performance(tree_scores, "tpr", "fpr")

plot(tree_perf,
     main="classification tree ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")

# AUC
tree_auc <- performance(tree_scores, "auc")
tree_auc2<- as.numeric(tree_auc@y.values)

# Tree visualization
# all training 

tree1 <- rpart(training.y~., trainx)
rpart.plot(tree1) 


# Prints
cm.tree2
cat("AUC Value for Classfication Tree is: ",tree_auc2)  ##AUC Value


