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
# load missy data to show messy visualizations
yelp<- read.csv("yelp_business_attributes.csv")

############################ Visualizations ################################
# 1. Preprocessing Visualization
library(ggplot2)
library(gridExtra)

# Bar Plot Showing The amount of missing data compared to what we have

p1<- ggplot(yelp, aes(x=ByAppointmentOnly))+
        geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill="steelblue")+
        scale_y_continuous(labels=scales::percent) +
        ylab("Percent")+
        theme_minimal()

p2<- ggplot(yelp, aes(x=BusinessAcceptsCreditCards))+
        geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill="steelblue")+
        scale_y_continuous(labels=scales::percent) +
        ylab("Percent")+
        theme_minimal()

p3<- ggplot(yelp, aes(x=BusinessParking_validated))+
        geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill="steelblue")+
        scale_y_continuous(labels=scales::percent) +
        ylab("Percent")+
        theme_minimal()

p4<- ggplot(yelp, aes(x=GoodForKids))+
        geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill="steelblue")+
        scale_y_continuous(labels=scales::percent) +
        ylab("Percent")+
        theme_minimal()

p5<- ggplot(yelp, aes(x=WheelchairAccessible))+
        geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill="steelblue")+
        scale_y_continuous(labels=scales::percent) +
        ylab("Percent")+
        theme_minimal()

p6<- ggplot(yelp, aes(x=Caters))+
        geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill="steelblue")+
        scale_y_continuous(labels=scales::percent) +
        ylab("Percent")+
        theme_minimal()

p7<- ggplot(yelp, aes(x=WiFi))+
        geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill="steelblue")+
        scale_y_continuous(labels=scales::percent) +
        ylab("Percent")+
        theme_minimal()

p8<- ggplot(yelp, aes(x=Smoking))+
        geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill="steelblue")+
        scale_y_continuous(labels=scales::percent) +
        ylab("Percent")+
        theme_minimal()

p9<- ggplot(yelp, aes(x=Open24Hours))+
        geom_bar(aes(y = (..count..)/sum(..count..)), width=0.3, fill="steelblue")+
        scale_y_continuous(labels=scales::percent) +
        ylab("Percent")+
        theme_minimal()

grid.arrange(p1, p2,p3,p4, p5,p6,p7,p8,p9,nrow = 3)


# 2. visualization of some predictors 


p1<-ggplot(data = training) +
        geom_bar(aes(x = factor(training.y), fill = factor(WheelchairAccessible)), position = "fill",width = 0.3)+
        xlab("Restaurant")+
        ylab("Ratios")+
        labs(fill="WheelchairAccessible") # Good (most low star resturants does not have valet, so it could be a good predictor)

p2<-ggplot(data = training) +
        geom_bar(aes(x = factor(training.y), fill = factor(BusinessParking_valet)), position = "fill",width = 0.3)+
        xlab("Restaurant")+
        ylab("Ratios")+
        labs(fill="BusinessParking_valet") # Good (most low star resturants does not have valet, so it could be a good predictor)

p3<-ggplot(data = training) +
        geom_bar(aes(x = factor(training.y), fill = factor(BikeParking)), position = "fill",width = 0.3)+
        xlab("Restaurant")+
        ylab("Ratios")+
        labs(fill="BikeParking") # Good (most low star resturants does not have valet, so it could be a good predictor)

p4<-ggplot(data = training) +
        geom_bar(aes(x = factor(training.y), fill = factor(GoodForMeal_breakfast)), position = "fill",width = 0.3)+
        xlab("Restaurant")+
        ylab("Ratios")+
        labs(fill="GoodForMeal_breakfast") # Good (most low star resturants does not have valet, so it could be a good predictor)

p5<-ggplot(data = training) +
        geom_bar(aes(x = factor(training.y), fill = factor(GoodForMeal_lunch)), position = "fill",width = 0.3)+
        xlab("Restaurant")+
        ylab("Ratios")+
        labs(fill="GoodForMeal_lunch") # Good (most low star resturants does not have valet, so it could be a good predictor)

p6<-ggplot(data = training) +
        geom_bar(aes(x = factor(training.y), fill = factor(GoodForMeal_dinner)), position = "fill",width = 0.3)+
        xlab("Restaurant")+
        ylab("Ratios")+
        labs(fill="GoodForMeal_dinner") # Good (most low star resturants does not have valet, so it could be a good predictor)


p7<-ggplot(data = training) +
        geom_bar(aes(x = factor(training.y), fill = factor(OutdoorSeating)), position = "fill",width = 0.3)+
        xlab("Restaurant")+
        ylab("Ratios")+
        labs(fill="OutdoorSeating") # Good (most low star resturants does not have valet, so it could be a good predictor)


p8<-ggplot(data = training) +
        geom_bar(aes(x = factor(training.y), fill = factor(DogsAllowed)), position = "fill",width = 0.3)+
        xlab("Restaurant")+
        ylab("Ratios")+
        labs(fill="DogsAllowed") # Good (most low star resturants does not have valet, so it could be a good predictor)

p9<-ggplot(data = training) +
        geom_bar(aes(x = factor(training.y), fill = factor(CoatCheck)), position = "fill",width = 0.3)+
        xlab("Restaurant")+
        ylab("Ratios")+
        labs(fill="CoatCheck") # Good (most low star resturants does not have valet, so it could be a good predictor)


grid.arrange(p1, p2,p3,p4, p5,p6,p7,p8,p9,nrow = 3)

