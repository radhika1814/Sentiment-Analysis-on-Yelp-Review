######################################Sentimental Analysis using Text Mining and Naive Bayes################################

library(tm)
library(e1071)
library(dplyr)
library(caret)
library(naivebayes)
#library(kernlab)
library(party)
library(FSelector)
library(rminer)
library(SnowballC)
library(RWeka)
library(wordcloud)

df <- read.csv("NY_IL_review.csv", stringsAsFactors = FALSE)
df <- df[1:10000,]

df = subset(df, select = -c(X, business_id))
set.seed(1)
#glimpse(df)
df <- df[sample(nrow(df)), ]
#glimpse(df)

df$review <- as.factor(df$review)

corpus <- Corpus(VectorSource(df$text))
corpus 
#inspect(corpus[1:3])
# Use dplyr's  %>% (pipe) utility to do this neatly.
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stemDocument, language = 'en') %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

dtm <- DocumentTermMatrix(corpus.clean)
dtm
#inspect(dtm[40:50, 10:15])

df.train <- df[1:7000,]
df.test <- df[7001:10000,]

dtm.train <- dtm[1:7000,]
dtm.test <- dtm[7001:10000,]

#dtm.test
corpus.clean.train <- corpus.clean[1:7000]
corpus.clean.test <- corpus.clean[7001:10000]

#dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

#dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

#dim(dtm.test.nb)
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

#model <- naiveBayes(as.matrix(trainNB),as.factor(df.train$review))
model <- naiveBayes(trainNB, df.train$review, laplace = c(1))
pred <- predict(model, newdata = testNB)
#results <- predict(model,as.matrix(dtm.test.nb))
#View(results)
#confusionMatrix(results, df.test$review)
testResultsNB <- data.frame(obs = df.test$review, pred = pred)
NB.summary <- defaultSummary(testResultsNB)
NB.summary
confusionMatrix(pred, df.test$review, positive = "Good")
#################################################################################

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
test.converted<-sapply(testResultsNB$obs,goodbad) 
pred.converted<-sapply(testResultsNB$pred,goodbad)


# roc


scores <- prediction(pred.converted, test.converted)


perf <- performance(scores, "tpr", "fpr")

plot(perf,
     main="Review Analysisc (Naive Bayes) ROC Curve",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")


