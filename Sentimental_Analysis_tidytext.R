################################Sentimental Analysis using TidyText#####################
library(tm)  #general text mining functions
library(DT)       # table format display of data
library(SnowballC) #for stemming
library('wordcloud')
library("tidytext")
library("RColorBrewer")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("ColorBrewer")
library("dplyr")
library(plyr)
library("ggplot2")
install.packages("textcat")
library("textcat")
reviews<-read.csv("reviews.csv")

##top 10 restaurant with reviews rated 5 stars
most5StarsReviews = reviews %>%
  filter(stars == 5) %>%
  group_by(business_id) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(BusinessID = reorder(business_id,Count)) %>%
  head(10)
View(most5StarsReviews)
most5StarsReviews = inner_join(most5StarsReviews,reviews)

most5StarsReviews %>%
  mutate(name = reorder(name,Count)) %>%
  ggplot(aes(x = name,y = Count)) +
  geom_bar(stat='identity',colour="blue", fill= 'black') +
  geom_text(aes(x = name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of the Business', 
       y = 'Count', 
       title = 'Name of the Business and Count') +
  coord_flip() +
  theme_bw()
##wordcloud for top restaurant

write.csv(most5StarsReviews, file="most5.csv")
most5 <- read.csv('most5.csv', stringsAsFactors = FALSE)
View(most5)
textCorpus <- Corpus(VectorSource(most5$text))
textCorpus <- tm_map(textCorpus, PlainTextDocument)
textCorpus <- tm_map(textCorpus, removePunctuation)
textCorpus <- tm_map(textCorpus, removeWords, stopwords('english'))
textCorpus <- tm_map(textCorpus, stemDocument)
textCorpus <- tm_map(textCorpus, removeWords,c("the","its","this","and","you", "got","The","the"))

textCorpus <- Corpus(VectorSource(textCorpus))
matrix_terms <- DocumentTermMatrix(textCorpus)

wordcloud(textCorpus, max.words=200, colors=brewer.pal(8, "Dark2"))

littlemissbbq = most5 %>%
  filter(name == "Little Miss BBQ")
matrix_terms <- DocumentTermMatrix(littlemissbbq)
 wordcloud(littlemissbbq)


##to sum the checkins
checkin<-read.csv("C:/Users/HP/Desktop/checkin.csv")
View(checkin)
checkin2 <- (aggregate(checkin$checkins, by=list(business_id=checkin$business_id), FUN=sum))
View(checkin2)

##top 10 restaurant with max checkins
mostcheckin = reviews1 %>%
 filter(sumcheckin > 3638 ) %>%
 group_by(business_id) %>%
 summarise(Count = n()) %>%
 arrange(desc(Count)) %>%
 ungroup() %>%
 mutate(BusinessID = reorder(business_id,Count)) %>%
 head(10)
View(mostcheckin)
 mostcheckin = inner_join(mostcheckin,reviews1)
 
 mostcheckin %>%
   mutate(name = reorder(name,sumcheckin)) %>%
   ggplot(aes(x = name,y = sumcheckin)) +
   geom_bar(stat='identity',colour="blue", fill= 'black') +
   geom_text(aes(x = name, y = sumcheckin, label = paste0("(",sumcheckin,")",sep="")),
             hjust=1, vjust=.5, size = 8, colour = 'red') +
   labs(x = 'Name of the Business', 
        y = 'Count', 
        title = 'Name of the Business and Count of checkins') +
   coord_flip() +
   theme_bw()


littlemissbbq = reviews %>% 
  filter(business_id == "Xg5qEQiB-7L6kGJ5F4K3bQ") %>%
  select(business_id,name,city,state,categories,text)
View(littlemissbbq)

datatable(head(littlemissbbq), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
write.csv(littlemissbbq, file="lmb.csv")
lmb<- read.csv("lmb.csv", stringsAsFactors = FALSE)
#top 10 most common words of little miss bbq
lmb %>%
  filter(business_id == "Xg5qEQiB-7L6kGJ5F4K3bQ") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10)%>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill ="lightblue") +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()
 

#positive and not so positive words of lmb reviews
positiveWordsBarGraph <- function(lmb) {
  contributions <- lmb %>%
    unnest_tokens(word, text) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
  
  contributions %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(20) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw()
}

positiveWordsBarGraph(lmb %>%
                        filter(business_id == "Xg5qEQiB-7L6kGJ5F4K3bQ"))


#Sentiment scores for little miss bbq

calculate_sentiment <- function(review_text)
{
  sentiment_lines  =  review_text %>%
    filter(textcat(text) == "english") %>%  # considering only English text
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(X) %>%
    summarize(sentiment = mean(score),words = n()) %>%
    ungroup() %>%
    filter(words >= 5)
  
  return(sentiment_lines)
  
}


sentiment_lines = calculate_sentiment(lmb)

head(sentiment_lines)

#negative reviews
display_neg_sentiments <- function(sentiment_lines,review_text)
{
  neg_sentiment_lines = sentiment_lines %>%
    arrange(desc(sentiment))  %>%
    top_n(-10, sentiment) %>%
    inner_join(review_text, by = "X") %>%
    select(sentiment,text) 
  
  datatable(neg_sentiment_lines, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
}

display_neg_sentiments(sentiment_lines,lmb)


#most 2 star reviews
most2StarsReviews = reviews %>%
  filter(stars == 2) %>%
  group_by(business_id) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(BusinessID = reorder(business_id,Count)) %>%
  head(10)
View(most2StarsReviews)
most2StarsReviews = inner_join(most2StarsReviews,reviews)

most2StarsReviews %>%
  mutate(name = reorder(name,Count)) %>%
  ggplot(aes(x = name,y = Count)) +
  geom_bar(stat='identity',colour="blue", fill= 'black') +
  geom_text(aes(x = name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of the Business', 
       y = 'Count', 
       title = 'Top ten Restaurants with 2 star reviews') +
  coord_flip() +
  theme_bw()

 #wordcloud ofmost 2
write.csv(most2StarsReviews, file="most2.csv")
most2<- read.csv('most2.csv')
View(most2)
textCorpus <- Corpus(VectorSource(most2$text))
textCorpus <- tm_map(textCorpus, PlainTextDocument)
textCorpus <- tm_map(textCorpus, removePunctuation)
textCorpus <- tm_map(textCorpus, removeWords, stopwords('english'))
textCorpus <- tm_map(textCorpus, stemDocument)
textCorpus <- tm_map(textCorpus, removeWords,c("the","its","this","and","you", "got","The","the"))

textCorpus <- Corpus(VectorSource(textCorpus))
matrix_terms <- DocumentTermMatrix(textCorpus)


wordcloud(textCorpus, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))


noodleshop = reviews %>% 
  filter(business_id == "IImZFNzX4cP84rFsyb-9mQ") %>%
  select(X,business_id,name,city,state,categories,text)
View(noodleshop)
write.csv(noodleshop, file="noodleshop.csv")
noodleshop<- read.csv("noodleshop.csv", stringsAsFactors = FALSE)
View(noodleshop)


#top 10 most common words of noodleshop
noodleshop %>%
  filter(business_id == "IImZFNzX4cP84rFsyb-9mQ") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10)%>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill ="lightblue") +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Top 10 most common words of Noodle shop') +
  coord_flip() + 
  theme_bw()

#positive and not so positive words of thenoodleshop reviews
positiveWordsBarGraph <- function(NS) {
  contributions <- NS %>%
    unnest_tokens(word, text) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
  
  contributions %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(20) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw()
}

positiveWordsBarGraph(noodleshop %>%
                        filter(business_id == "IImZFNzX4cP84rFsyb-9mQ"))

#Sentiment scores for noodleshop

calculate_sentiment <- function(review_text)
{
  sentiment_lines  =  review_text %>%
    filter(textcat(text) == "english") %>%  # considering only English text
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    #group_by(X) %>%
    summarize(sentiment = mean(score),words = n()) %>%
    ungroup() %>%
    filter(words >= 5)
  
  return(sentiment_lines)
  
}
library(textcat)

sentiment_lines = calculate_sentiment(noodleshop)

head(sentiment_lines)


#negative reviews
display_neg_sentiments <- function(sentiment_lines,review_text)
{
  neg_sentiment_lines = sentiment_lines %>%
    arrange(desc(sentiment))  %>%
    top_n(-10, sentiment) %>%
    inner_join(review_text, by = "X") %>%
    select(sentiment,text) 
  
  datatable(neg_sentiment_lines, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
}

display_neg_sentiments(sentiment_lines,noodleshop)




