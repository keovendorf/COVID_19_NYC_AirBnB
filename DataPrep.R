library(dplyr)
library(lubridate)
library(tidyverse)
library(caret)
library(car)
library(pROC)
library(Hmisc)
setwd("/Users/bdign/Documents/R/AIR")

listings <- read.csv("listings.csv", stringsAsFactors = F, na.strings = c("")) 
calendar <- read.csv("calendar.csv", stringsAsFactors = F, na.strings = c("")) 
reviews <- read.csv("reviews.csv", stringsAsFactors = F, na.strings = c(""))


listings [c(2:19, 21:22, 24:26, 30:31, 35:36)] <- list(NULL)
listings [c(7,11:13,15:21,25,31,32,35,36,37,39,40,42,43,44)] <- list(NULL)
listings [c(23:28, 33, 36,37,46:49)] <- list(NULL)
listings [c(36, 38:43)] <- list(NULL)
listings [c(23:26)] <- list(NULL)
listings [c(2:3)] <- list(NULL)
listings [c(6,9, 10,11, 21 ,24,26,27,31)] <- list(NULL)

##Clean Listings####
listings$host_is_superhost   <- as.factor(listings$host_is_superhost)
listings$neighbourhood_group_cleansed   <- as.factor(listings$neighbourhood_group_cleansed)
listings$cancellation_policy   <- as.factor(listings$cancellation_policy)
listings$price  <- as.numeric(gsub('[$,]', '', listings$price))
listings$cleaning_fee <- as.numeric(gsub('[$,]', '', listings$cleaning_fee))
listings$host_response_rate <- as.numeric(gsub('[%,]', '', listings$host_response_rate))
listings$host_acceptance_rate <- as.numeric(gsub('[%,]', '', listings$host_acceptance_rate))
listings$room_type <-as.factor(listings$room_type)
listings$host_identity_verified <-as.factor(listings$host_identity_verified)
###Remove outliers###
listings <- listings %>% filter(host_listings_count<200) %>% filter(bathrooms<7)%>% 
            filter(price<5000)%>% filter(minimum_nights<370)



reviews$date <- as.Date(reviews$date)
reviews$listing_id <- as.factor(reviews$listing_id)
reviews$id <- as.factor(reviews$id)



#Pre-Covid 20
pre20 <- reviews %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2020-03-13")) 
pre20c <- data.frame(table(pre20$listing_id))
colnames(pre20c) <- c("id", "Pre_20_reviews")
listings = merge(x=listings, y=pre20c,by="id", all=TRUE)


#Post-Covid 20
post20 <- reviews %>%
  filter(date >= as.Date("2020-03-14") & date <= as.Date("2020-06-10")) 
post20c <- data.frame(table(post20$listing_id))
colnames(post20c) <- c("id", "Post_20_reviews")
listings = merge(x=listings, y=post20c,by="id", all=TRUE)



###CALENDAR WORK ####

##Eliminate dates later than End of Sept###

calendar <- calendar %>%
  filter(date >= as.Date("2020-06-09") & date <= as.Date("2020-09-30"))

calendar [c(4:7)] <- list(NULL)
calendar$date <- as.Date(calendar$date)
calendar$listing_id <- as.factor(calendar$listing_id)
calendar$available <- as.factor(calendar$available)



###Count available/not available dates###

calendar <- calendar %>% group_by(listing_id) %>% count(available)

hist(calendar$n)

###Isolate those that aren't for rent at all####
calendarf <- calendar %>% filter(n == 114) %>% filter(available == 'f')
calendarf [c(3)] <- list(NULL)
colnames(calendarf) = c("id", "ForRent")


###Isolate those that are likely for rent####
calendart <- calendar %>% filter(n >55) %>% filter(available == 't')
calendart [c(3)] <- list(NULL)
colnames(calendart) = c("id", "ForRent")

###Merge back into listings###
notlisted = merge(x=listings, y=calendarf,by="id")
listed = merge(x=listings, y=calendart,by="id")


###Units fully listed but haven't received a single review since COVID###
##Renters don't like these units?  - Demand Issue?##
CantRent <- listed %>% filter(ForRent == 't') %>% filter(Post_20_reviews == 0)

###These are the one that are listed for rent and have been rented##
Liked <- listed %>% filter(ForRent == 't') %>% filter(Post_20_reviews >= 1)

##Properties with at least 2 reviews pre-Covid 2020 but now not listed at all ###
###Landlords not willing to list these units?  - Supply Issue? ####
Pulled <- notlisted %>% filter(ForRent == 'f') %>% filter(Pre_20_reviews >= 1)

####Pull comments back in from Reviews ####

reviews [c(2:5)] <- list(NULL)
colnames(reviews) = c("id", "reviewtext")

###All reviews, all time periods of listings pulled off Post Covid###
Pulledreview = merge(x=Pulled, y=reviews,by="id")
Pulledreview [c(2:36)] <- list(NULL)

###All reviews, all time periods of listings that couldn't rent Post-Covid###
CantRentReview = merge(x=CantRent, y=reviews,by="id")
CantRentReview [c(2:36)] <- list(NULL)

###All reviews, all time periods of listings that did rent Post-Covid###
LikedReview = merge(x=Liked, y=reviews,by="id")
LikedReview [c(2:36)] <- list(NULL)

listed1 <- listed

listed1 [c(1, 22, 23,25)] <- list(NULL)
###Make Post_Covid_reviews binary, with yes= 1 and no=0###
listed1$Post_20_reviews <- ifelse(listed1$Post_20_reviews == 0, 2, 1)  
listed1$Post_20_reviews <- ifelse(listed1$Post_20_reviews == 2, 0, 1)  


listed1$Post_20_reviews <-as.factor(listed1$Post_20_reviews)
listed1$host_acceptance_rate <- with(listed1,impute(host_acceptance_rate,mean))
listed1$host_response_rate <- with(listed1,impute(host_response_rate,mean))
listed1$host_listings_count <- with(listed1,impute(host_listings_count,mean))
listed1$bathrooms <- with(listed1,impute(bathrooms,mean))
listed1$bedrooms <- with(listed1,impute(bedrooms,mean))
listed1$beds <- with(listed1,impute(beds,mean))
listed1$square_feet <- with(listed1,impute(square_feet,mean))
listed1$cleaning_fee <- with(listed1,impute(cleaning_fee,mean))
listed1$review_scores_rating <- with(listed1,impute(review_scores_rating,mean))
listed1$review_scores_cleanliness <- with(listed1,impute(review_scores_cleanliness,mean))
listed1$review_scores_location <- with(listed1,impute(review_scores_location,mean))
listed1$review_scores_value <- with(listed1,impute(review_scores_value,mean))
listed1 <- na.omit(listed1)

calendar1 <- calendar
calendar1 [c(3)] <- list(NULL)
colnames(calendar1) = c("id", "ForRent")
listings2 = merge(x=listings, y=calendar1,by="id")

listings2 [c(1, 22, 23,24)] <- list(NULL)

listings2$ForRent <- ifelse(listings2$ForRent == "t", 1, 0)  


listings2$ForRent <-as.factor(listings2$ForRent)
listings2$host_acceptance_rate <- with(listings2,impute(host_acceptance_rate,mean))
listings2$host_response_rate <- with(listings2,impute(host_response_rate,mean))
listings2$host_listings_count <- with(listings2,impute(host_listings_count,mean))
listings2$bathrooms <- with(listings2,impute(bathrooms,mean))
listings2$bedrooms <- with(listings2,impute(bedrooms,mean))
listings2$beds <- with(listings2,impute(beds,mean))
listings2$square_feet <- with(listings2,impute(square_feet,mean))
listings2$cleaning_fee <- with(listings2,impute(cleaning_fee,mean))
listings2$review_scores_rating <- with(listings2,impute(review_scores_rating,mean))
listings2$review_scores_cleanliness <- with(listings2,impute(review_scores_cleanliness,mean))
listings2$review_scores_location <- with(listings2,impute(review_scores_location,mean))
listings2$review_scores_value <- with(listings2,impute(review_scores_value,mean))
listings2 <- na.omit(listings2)

summary(listings2)






write.csv(listed1,'ModelData4Demand.csv')
write.csv(listings2,'ModelData4Supply.csv')
