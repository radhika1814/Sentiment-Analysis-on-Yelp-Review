install.packages("data.table")

install.packages("mice")

setwd("C:/Mason/SYST 568 - Applied Predictive/Project Proposal/data/drive-download-20180306T173924Z-001")
library(mice)
library(data.table)
library(dplyr)
library(tidyverse) 
library(tidyr)

####Read all files##########
yelp_business <- read.csv("yelp_business.csv", stringsAsFactors=FALSE)
yelp_business_attributes <- read.csv("yelp_business_attributes.csv", stringsAsFactors=FALSE)

#########Merge all business files######
business <- merge(x=yelp_business,y=yelp_business_attributes, by = "business_id", all.y=TRUE)

###Filter only "Restaurants" business####
Yelp_Restaurants <-business[business$categories %like% "Restaurants", ]

####Filter only US Restaurants
Yelp_US_Restaurants <-Yelp_Restaurants[!(Yelp_Restaurants$state %in% c("BC", "MB", "NB", "NL", "NS", "NT", "NU","ON", "PE", "QC","SK","YT")),]

####Convert Na or None values to NA####
Yelp_US_Restaurants[ Yelp_US_Restaurants == "Na" ] <- NA

##Removing Hair Related Attribtues
Yelp_US_Restaurants <- Yelp_US_Restaurants %>% select(-contains("HairSpecializesIn"))

##Remove columns with all missing values
allmisscols <- apply(Yelp_US_Restaurants,2, function(x)all(is.na(x)));  
colswithallmiss <-names(allmisscols[allmisscols>0]);  

###Column names with all NAs
colswithallmiss

Yelp_US_Restaurants <- subset( Yelp_US_Restaurants, select = -c(AcceptsInsurance, Corkage, DietaryRestrictions_dairy.free,ByAppointmentOnly, RestaurantsPriceRange2, BYOBCorkage, RestaurantsCounterService  ) )

####Find count of NAs in all columns
colSums(is.na(Yelp_US_Restaurants))

###Remove the records where restaurants is not open
Yelp_US_Restaurants <- Yelp_US_Restaurants[Yelp_US_Restaurants$is_open == "1" ,]


###Include only relevant attributes where number of NAs is less than or equal to 96%
Yelp_US_Restaurants <- subset( Yelp_US_Restaurants, select = c(business_id,name, categories, city, state, stars, review_count,BusinessParking_street, BusinessParking_validated, BusinessParking_lot, BusinessParking_valet, BikeParking, Caters, WheelchairAccessible, OutdoorSeating, GoodForMeal_latenight, GoodForMeal_lunch, GoodForMeal_dinner, GoodForMeal_breakfast, GoodForMeal_brunch, CoatCheck, DogsAllowed)) 

##Exclude rows with NA values; all the rows have atleast one column with NA value
na.exclude (Yelp_US_Restaurants)

##Convert true to 1 and false to 0
Yelp_US_Restaurants[Yelp_US_Restaurants=="True"]<-1

Yelp_US_Restaurants[Yelp_US_Restaurants=="False"]<-0

table(Yelp_US_Restaurants$DogsAllowed)

#P7 <- 100 * length(which(Yelp_US_Restaurants$DogsAllowed == 0)) / length(Yelp_US_Restaurants$DogsAllowed)
#write.csv(Yelp_US_Restaurants, file="Yelp_US_Restaurants_NA.csv")

###Convert all character variables to factors
character_vars <- lapply(Yelp_US_Restaurants, class) == "character"

Yelp_US_Restaurants[, character_vars] <- lapply(Yelp_US_Restaurants[, character_vars], as.factor)

summary(Yelp_US_Restaurants)

####Impute Parking Attributes
Yelp_US_Restaurants_1star <- Yelp_US_Restaurants[Yelp_US_Restaurants$stars %in% c("1","1.5") ,]
parking_attribs <- subset( Yelp_US_Restaurants_1star, select = c(business_id,name, city, state, BusinessParking_street, BusinessParking_validated, BusinessParking_lot, BusinessParking_valet)) 

parking_attribs$BusinessParking_street<-as.factor(parking_attribs$BusinessParking_street)
parking_attribs$BusinessParking_validated<-as.factor(parking_attribs$BusinessParking_validated)
parking_attribs$BusinessParking_lot<-as.factor(parking_attribs$BusinessParking_lot)
parking_attribs$BusinessParking_valet<-as.factor(parking_attribs$BusinessParking_valet)

memory.limit(80000)

imputed_parking_attribs <- mice(parking_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_parking_attribs <- mice::complete(imputed_parking_attribs,1)

####Impute Other business Attributes
other_attribs <- subset( Yelp_US_Restaurants_1star, select = c(business_id,name, city, state, CoatCheck, DogsAllowed, BikeParking, Caters, WheelchairAccessible, OutdoorSeating)) 
#other_attribs <- subset( Yelp_US_Restaurants, select = c(business_id,name, city, state, stars, CoatCheck, DogsAllowed)) 
other_attribs$BikeParking<-as.factor(other_attribs$BikeParking)
other_attribs$Caters<-as.factor(other_attribs$Caters)
other_attribs$WheelchairAccessible<-as.factor(other_attribs$WheelchairAccessible)
other_attribs$OutdoorSeating<-as.factor(other_attribs$OutdoorSeating)
other_attribs$CoatCheck<-as.factor(other_attribs$CoatCheck)
other_attribs$DogsAllowed<-as.factor(other_attribs$DogsAllowed)

imputed_other_attribs <- mice(other_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_other_attribs <- mice::complete(imputed_other_attribs,1)

####Impute Good for Meal Attributes
Meal_attribs <- subset( Yelp_US_Restaurants_1star, select = c(business_id,name, city, state, GoodForMeal_latenight, GoodForMeal_dinner, GoodForMeal_breakfast, GoodForMeal_brunch,GoodForMeal_lunch)) 

Meal_attribs$GoodForMeal_latenight<-as.factor(Meal_attribs$GoodForMeal_latenight)
Meal_attribs$GoodForMeal_dinner<-as.factor(Meal_attribs$GoodForMeal_dinner)
Meal_attribs$GoodForMeal_breakfast<-as.factor(Meal_attribs$GoodForMeal_breakfast)
Meal_attribs$GoodForMeal_brunch<-as.factor(Meal_attribs$GoodForMeal_brunch)
Meal_attribs$GoodForMeal_lunch<-as.factor(Meal_attribs$GoodForMeal_lunch)

imputed_meal_attribs <- mice(Meal_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_meal_attribs <- mice::complete(imputed_meal_attribs,1)

####Merge imputed attributes
UnImputedDataSet <- subset( Yelp_US_Restaurants_1star, select = c(business_id,name, categories,city, state, stars, review_count)) 

Final_data <- merge(x=UnImputedDataSet,y=imputed_parking_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Final_data <- merge(x=Final_data,y=imputed_other_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Yelp_US_Restaurants_1star <- merge(x=Final_data,y=imputed_meal_attribs, by = c("business_id","city","state","name"), all.x = TRUE)


####Impute Parking Attributes for 2 star restaurants
Yelp_US_Restaurants_2star <- Yelp_US_Restaurants[Yelp_US_Restaurants$stars %in% c("2","2.5") ,]
parking_attribs <- subset( Yelp_US_Restaurants_2star, select = c(business_id,name, city, state, BusinessParking_street, BusinessParking_validated, BusinessParking_lot, BusinessParking_valet)) 

parking_attribs$BusinessParking_street<-as.factor(parking_attribs$BusinessParking_street)
parking_attribs$BusinessParking_validated<-as.factor(parking_attribs$BusinessParking_validated)
parking_attribs$BusinessParking_lot<-as.factor(parking_attribs$BusinessParking_lot)
parking_attribs$BusinessParking_valet<-as.factor(parking_attribs$BusinessParking_valet)

imputed_parking_attribs <- mice(parking_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_parking_attribs <- mice::complete(imputed_parking_attribs,1)

####Impute Other business Attributes
other_attribs <- subset( Yelp_US_Restaurants_2star, select = c(business_id,name, city, state, CoatCheck, DogsAllowed, BikeParking, Caters, WheelchairAccessible, OutdoorSeating)) 
#other_attribs <- subset( Yelp_US_Restaurants, select = c(business_id,name, city, state, stars, CoatCheck, DogsAllowed)) 
other_attribs$BikeParking<-as.factor(other_attribs$BikeParking)
other_attribs$Caters<-as.factor(other_attribs$Caters)
other_attribs$WheelchairAccessible<-as.factor(other_attribs$WheelchairAccessible)
other_attribs$OutdoorSeating<-as.factor(other_attribs$OutdoorSeating)
other_attribs$CoatCheck<-as.factor(other_attribs$CoatCheck)
other_attribs$DogsAllowed<-as.factor(other_attribs$DogsAllowed)

memory.limit(30000)
imputed_other_attribs <- mice(other_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_other_attribs <- mice::complete(imputed_other_attribs,1)

####Impute Good for Meal Attributes
Meal_attribs <- subset( Yelp_US_Restaurants_2star, select = c(business_id,name, city, state, GoodForMeal_lunch,GoodForMeal_latenight, GoodForMeal_dinner, GoodForMeal_breakfast, GoodForMeal_brunch)) 

Meal_attribs$GoodForMeal_latenight<-as.factor(Meal_attribs$GoodForMeal_latenight)
Meal_attribs$GoodForMeal_dinner<-as.factor(Meal_attribs$GoodForMeal_dinner)
Meal_attribs$GoodForMeal_breakfast<-as.factor(Meal_attribs$GoodForMeal_breakfast)
Meal_attribs$GoodForMeal_brunch<-as.factor(Meal_attribs$GoodForMeal_brunch)
Meal_attribs$GoodForMeal_lunch<-as.factor(Meal_attribs$GoodForMeal_lunch)

imputed_meal_attribs <- mice(Meal_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_meal_attribs <- mice::complete(imputed_meal_attribs,1)

####Merge imputed attributes
UnImputedDataSet <- subset( Yelp_US_Restaurants_2star, select = c(business_id,name, categories,city, state, stars, review_count)) 

Final_data <- merge(x=UnImputedDataSet,y=imputed_parking_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Final_data <- merge(x=Final_data,y=imputed_other_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Yelp_US_Restaurants_2star <- merge(x=Final_data,y=imputed_meal_attribs, by = c("business_id","city","state","name"), all.x = TRUE)



####Impute Parking Attributes for 3 star restaurants
Yelp_US_Restaurants_3star <- Yelp_US_Restaurants[Yelp_US_Restaurants$stars %in% c("3","3.5") ,]
parking_attribs <- subset( Yelp_US_Restaurants_3star, select = c(business_id,name, city, state, BusinessParking_street, BusinessParking_validated, BusinessParking_lot, BusinessParking_valet)) 

parking_attribs$BusinessParking_street<-as.factor(parking_attribs$BusinessParking_street)
parking_attribs$BusinessParking_validated<-as.factor(parking_attribs$BusinessParking_validated)
parking_attribs$BusinessParking_lot<-as.factor(parking_attribs$BusinessParking_lot)
parking_attribs$BusinessParking_valet<-as.factor(parking_attribs$BusinessParking_valet)


imputed_parking_attribs <- mice(parking_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_parking_attribs <- mice::complete(imputed_parking_attribs,1)

####Impute Other business Attributes
other_attribs <- subset( Yelp_US_Restaurants_3star, select = c(business_id,name, city, state, CoatCheck, DogsAllowed, BikeParking, Caters, WheelchairAccessible, OutdoorSeating)) 
#other_attribs <- subset( Yelp_US_Restaurants, select = c(business_id,name, city, state, stars, CoatCheck, DogsAllowed)) 
other_attribs$BikeParking<-as.factor(other_attribs$BikeParking)
other_attribs$Caters<-as.factor(other_attribs$Caters)
other_attribs$WheelchairAccessible<-as.factor(other_attribs$WheelchairAccessible)
other_attribs$OutdoorSeating<-as.factor(other_attribs$OutdoorSeating)
other_attribs$CoatCheck<-as.factor(other_attribs$CoatCheck)
other_attribs$DogsAllowed<-as.factor(other_attribs$DogsAllowed)

memory.limit(30000)
imputed_other_attribs <- mice(other_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_other_attribs <- mice::complete(imputed_other_attribs,1)

####Impute Good for Meal Attributes
Meal_attribs <- subset( Yelp_US_Restaurants_3star, select = c(business_id,name, city, state, GoodForMeal_latenight, GoodForMeal_dinner, GoodForMeal_breakfast, GoodForMeal_brunch, GoodForMeal_lunch)) 

Meal_attribs$GoodForMeal_latenight<-as.factor(Meal_attribs$GoodForMeal_latenight)
Meal_attribs$GoodForMeal_dinner<-as.factor(Meal_attribs$GoodForMeal_dinner)
Meal_attribs$GoodForMeal_breakfast<-as.factor(Meal_attribs$GoodForMeal_breakfast)
Meal_attribs$GoodForMeal_brunch<-as.factor(Meal_attribs$GoodForMeal_brunch)
Meal_attribs$GoodForMeal_lunch<-as.factor(Meal_attribs$GoodForMeal_lunch)

imputed_meal_attribs <- mice(Meal_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_meal_attribs <- mice::complete(imputed_meal_attribs,1)

####Merge imputed attributes
UnImputedDataSet <- subset( Yelp_US_Restaurants_3star, select = c(business_id,name, categories,city, state, stars, review_count)) 

Final_data <- merge(x=UnImputedDataSet,y=imputed_parking_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Final_data <- merge(x=Final_data,y=imputed_other_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Yelp_US_Restaurants_3star <- merge(x=Final_data,y=imputed_meal_attribs, by = c("business_id","city","state","name"), all.x = TRUE)



####Impute Parking Attributes for 4 star restaurants
Yelp_US_Restaurants_4star <- Yelp_US_Restaurants[Yelp_US_Restaurants$stars %in% c("4","4.5") ,]
parking_attribs <- subset( Yelp_US_Restaurants_4star, select = c(business_id,name, city, state, BusinessParking_street, BusinessParking_validated, BusinessParking_lot, BusinessParking_valet)) 

parking_attribs$BusinessParking_street<-as.factor(parking_attribs$BusinessParking_street)
parking_attribs$BusinessParking_validated<-as.factor(parking_attribs$BusinessParking_validated)
parking_attribs$BusinessParking_lot<-as.factor(parking_attribs$BusinessParking_lot)
parking_attribs$BusinessParking_valet<-as.factor(parking_attribs$BusinessParking_valet)


imputed_parking_attribs <- mice(parking_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_parking_attribs <- mice::complete(imputed_parking_attribs,1)

####Impute Other business Attributes
other_attribs <- subset( Yelp_US_Restaurants_4star, select = c(business_id,name, city, state, CoatCheck, DogsAllowed, BikeParking, Caters, WheelchairAccessible, OutdoorSeating)) 
#other_attribs <- subset( Yelp_US_Restaurants, select = c(business_id,name, city, state, stars, CoatCheck, DogsAllowed)) 
other_attribs$BikeParking<-as.factor(other_attribs$BikeParking)
other_attribs$Caters<-as.factor(other_attribs$Caters)
other_attribs$WheelchairAccessible<-as.factor(other_attribs$WheelchairAccessible)
other_attribs$OutdoorSeating<-as.factor(other_attribs$OutdoorSeating)
other_attribs$CoatCheck<-as.factor(other_attribs$CoatCheck)
other_attribs$DogsAllowed<-as.factor(other_attribs$DogsAllowed)

memory.limit(30000)
imputed_other_attribs <- mice(other_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_other_attribs <- mice::complete(imputed_other_attribs,1)

####Impute Good for Meal Attributes
Meal_attribs <- subset( Yelp_US_Restaurants_4star, select = c(business_id,name, city, state, GoodForMeal_latenight, GoodForMeal_dinner, GoodForMeal_breakfast, GoodForMeal_brunch, GoodForMeal_lunch)) 

Meal_attribs$GoodForMeal_latenight<-as.factor(Meal_attribs$GoodForMeal_latenight)
Meal_attribs$GoodForMeal_dinner<-as.factor(Meal_attribs$GoodForMeal_dinner)
Meal_attribs$GoodForMeal_breakfast<-as.factor(Meal_attribs$GoodForMeal_breakfast)
Meal_attribs$GoodForMeal_brunch<-as.factor(Meal_attribs$GoodForMeal_brunch)
Meal_attribs$GoodForMeal_lunch<-as.factor(Meal_attribs$GoodForMeal_lunch)

imputed_meal_attribs <- mice(Meal_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_meal_attribs <- mice::complete(imputed_meal_attribs,1)

####Merge imputed attributes
UnImputedDataSet <- subset( Yelp_US_Restaurants_4star, select = c(business_id,name, categories,city, state, stars, review_count)) 

Final_data <- merge(x=UnImputedDataSet,y=imputed_parking_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Final_data <- merge(x=Final_data,y=imputed_other_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Yelp_US_Restaurants_4star <- merge(x=Final_data,y=imputed_meal_attribs, by = c("business_id","city","state","name"), all.x = TRUE)




####Impute Parking Attributes for 5 star restaurants
Yelp_US_Restaurants_5star <- Yelp_US_Restaurants[Yelp_US_Restaurants$stars %in% c("5") ,]
parking_attribs <- subset( Yelp_US_Restaurants_5star, select = c(business_id,name, city, state, BusinessParking_street, BusinessParking_validated, BusinessParking_lot, BusinessParking_valet)) 

parking_attribs$BusinessParking_street<-as.factor(parking_attribs$BusinessParking_street)
parking_attribs$BusinessParking_validated<-as.factor(parking_attribs$BusinessParking_validated)
parking_attribs$BusinessParking_lot<-as.factor(parking_attribs$BusinessParking_lot)
parking_attribs$BusinessParking_valet<-as.factor(parking_attribs$BusinessParking_valet)


imputed_parking_attribs <- mice(parking_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_parking_attribs <- mice::complete(imputed_parking_attribs,1)

####Impute Other business Attributes
other_attribs <- subset( Yelp_US_Restaurants_5star, select = c(business_id,name, city, state, CoatCheck, DogsAllowed, BikeParking, Caters, WheelchairAccessible, OutdoorSeating)) 
#other_attribs <- subset( Yelp_US_Restaurants, select = c(business_id,name, city, state, stars, CoatCheck, DogsAllowed)) 
other_attribs$BikeParking<-as.factor(other_attribs$BikeParking)
other_attribs$Caters<-as.factor(other_attribs$Caters)
other_attribs$WheelchairAccessible<-as.factor(other_attribs$WheelchairAccessible)
other_attribs$OutdoorSeating<-as.factor(other_attribs$OutdoorSeating)
other_attribs$CoatCheck<-as.factor(other_attribs$CoatCheck)
other_attribs$DogsAllowed<-as.factor(other_attribs$DogsAllowed)

memory.limit(30000)
imputed_other_attribs <- mice(other_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_other_attribs <- mice::complete(imputed_other_attribs,1)

####Impute Good for Meal Attributes
Meal_attribs <- subset( Yelp_US_Restaurants_5star, select = c(business_id,name, city, state, GoodForMeal_latenight, GoodForMeal_dinner, GoodForMeal_breakfast, GoodForMeal_brunch, GoodForMeal_lunch)) 

Meal_attribs$GoodForMeal_latenight<-as.factor(Meal_attribs$GoodForMeal_latenight)
Meal_attribs$GoodForMeal_dinner<-as.factor(Meal_attribs$GoodForMeal_dinner)
Meal_attribs$GoodForMeal_breakfast<-as.factor(Meal_attribs$GoodForMeal_breakfast)
Meal_attribs$GoodForMeal_brunch<-as.factor(Meal_attribs$GoodForMeal_brunch)
Meal_attribs$GoodForMeal_lunch<-as.factor(Meal_attribs$GoodForMeal_lunch)

imputed_meal_attribs <- mice(Meal_attribs,m=3,meth='pmm',maxit=10,seed=500)
imputed_meal_attribs <- mice::complete(imputed_meal_attribs,1)

####Merge imputed attributes
UnImputedDataSet <- subset( Yelp_US_Restaurants_5star, select = c(business_id,name, categories,city, state, stars, review_count)) 

Final_data <- merge(x=UnImputedDataSet,y=imputed_parking_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Final_data <- merge(x=Final_data,y=imputed_other_attribs, by = c("business_id","city","state","name"), all.x = TRUE)
Yelp_US_Restaurants_5star <- merge(x=Final_data,y=imputed_meal_attribs, by = c("business_id","city","state","name"), all.x = TRUE)

Yelp_US_Restaurants_Imputed <- rbind(Yelp_US_Restaurants_1star, Yelp_US_Restaurants_2star, Yelp_US_Restaurants_3star, Yelp_US_Restaurants_4star, Yelp_US_Restaurants_5star)

yelp_checkin <- read.csv("yelp_checkin.csv", stringsAsFactors=FALSE)
numberOfCheckins<-aggregate(checkins~business_id,data=yelp_checkin,FUN=sum, na.action = na.omit)

Yelp_US_Restaurants <- merge(x=Yelp_US_Restaurants_Imputed,y=numberOfCheckins, by = "business_id", all.x = TRUE)
Yelp_US_Restaurants_final <- na.omit(Yelp_US_Restaurants)

#Yelp_US_Restaurants$NumberOfCheckins <- NULL
colnames(Yelp_US_Restaurants_final)[colnames(Yelp_US_Restaurants_final) == 'checkins'] <- 'NumberOfCheckins'


write.csv(Yelp_US_Restaurants_final, file="Yelp.csv")
