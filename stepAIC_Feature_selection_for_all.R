install.packages("tidyverse")
install.packages("readr")
install.packages("leaps")
library(leaps)
library(readr)
library(tidyverse)
library(ggplot2)
library(caret)
library(leaps)
library(MASS)
getwd()

dat <- read.csv("Dallas_Aug_22.csv")
colnames(dat) 

#for Dallas

# selecting the neighbourhoods based on the dataset we are viewign 
#(Dallas currently, uncomment the other sections to do the same for Dallas and RhodeIsland)

data <- dat %>% dplyr::select(host_response_rate, host_acceptance_rate, 
                              host_is_superhost,host_identity_verified,
                              property_type, room_type,accommodates,
                              bathrooms_text,bedrooms,beds,price,
                              has_availability,number_of_reviews,
                              review_scores_rating,review_scores_accuracy,
                              review_scores_cleanliness,review_scores_checkin,
                              review_scores_communication,review_scores_location,
                              review_scores_value,host_response_time,neighbourhood_cleansed)

#for New York
#data$neighbourhood_groups <-NA
#data$neighbourhood_groups[data$neighbourhood_group_cleansed == 'Bronx']<- 1
#data$neighbourhood_groups[data$neighbourhood_group_cleansed == 'Brooklyn']<- 2
#data$neighbourhood_groups[data$neighbourhood_group_cleansed == 'Manhattan']<- 3
#data$neighbourhood_groups[data$neighbourhood_group_cleansed == 'Queens']<- 4
#data$neighbourhood_groups[data$neighbourhood_group_cleansed == 'Staten Island']<- 5



#for Dallas 

data$neighbourhood_groups <-NA
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 1']<- 1
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 2']<- 2
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 3']<- 3
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 4']<- 4
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 5']<- 5
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 6']<- 6
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 7']<- 7
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 8']<- 8
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 9']<- 9
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 10']<- 10
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 11']<- 11
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 12']<- 12
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 13']<- 13
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 14']<- 14
data$neighbourhood_groups[data$neighbourhood_cleansed == 'District 15']<- 15


#for Rhode Island

#data$neighbourhood_groups <-NA
#data$neighbourhood_groups[data$neighbourhood_cleansed == 'Newport']<- 1
#data$neighbourhood_groups[data$neighbourhood_cleansed == 'Washington']<- 2
#data$neighbourhood_groups[data$neighbourhood_cleansed == 'Bristol']<- 3
#data$neighbourhood_groups[data$neighbourhood_cleansed == 'Providence']<- 4
#data$neighbourhood_groups[data$neighbourhood_cleansed == 'Kent']<- 5



colnames(data)


#Preprocessing the data

data$price_numeric <- as.numeric(gsub('[$,]', '', dat$price))

#removing the maximum nights from data2
colnames(dat)
#converting bathroom)_text to numeric 
data$bathrooms_numeric<- lapply(data$bathrooms_text, function(x) as.numeric(sub("(^\\d+).*", "\\1", x)))

#converting t/f to 1/0
data$is_superhost <- ifelse(data$host_is_superhost == 't',1,0)
data$identity_verified <- ifelse(data$host_identity_verified == 't',1,0)
data$has_avail <- ifelse(data$has_availability == 't',1,0)

colnames(data)
#converting review to number from 1 through 5

data$review_scores_final[data$review_scores_rating <= 2.00]<-1
data$review_scores_final[data$review_scores_rating > 2.00 & data$review_scores_rating <= 4.00 ] <- 2
data$review_scores_final[data$review_scores_rating > 4.00 & data$review_scores_rating <= 4.50] <- 3
data$review_scores_final[data$review_scores_rating > 4.50 & data$review_scores_rating <= 4.80] <- 4
data$review_scores_final[data$review_scores_rating > 4.80] <- 5

#convertin the response time to numeric 


data$host_response_numeric[data$host_response_time == 'within an hour']<- 4
data$host_response_numeric[data$host_response_time == 'a few days or more']<- 1
data$host_response_numeric[data$host_response_time == 'within a day']<- 2
data$host_response_numeric[data$host_response_time == 'within a few hours']<- 3



data$property_numeric[data$room_type == 'Entire home/apt']<- 3
data$property_numeric[data$room_type == 'Private room']<- 2
data$property_numeric[data$room_type == 'Shared room']<- 1


data$response_rate <- as.numeric(gsub('[%,]', '', data$host_response_rate))
data$price_numeric <- as.numeric(gsub('[$,]', '', data$price))

#droppping all the NA values
#Make sure that the categorical data is treated as a categorical data (using as.factor() function for that)

data1 <- data

colnames(data1)
data1$property_numeric = as.factor(data1$property_numeric)
data1$neighbourhood_groups = as.factor(data1$neighbourhood_groups)
data1$host_response_numeric = as.factor(data1$host_response_numeric)

#selecting the columns to build later the linear model on
#feature selection 
data1 <- data1 %>% dplyr::select(accommodates,beds,
                                 number_of_reviews,bathrooms_numeric,review_scores_final,
                                 property_numeric,response_rate,price_numeric,neighbourhood_groups)

colnames(data1)


data1$bathrooms_numeric <- sapply(data1$bathrooms_numeric, as.numeric)

# Making sure there are no NA values - substituting those with 0 


data1$bathrooms_numeric[data1$bathrooms_numeric == 'NA']<- 0
data1$bathrooms_numeric[data1$bathrooms_numeric == '-Inf']<- 0
data1$response_rate[data1$response_rate == '-Inf']<- 0


colnames(data1)

# Creating the multiple linear regression model

full.model <- lm(price_numeric ~., data = data1)

step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)


#best features

models <- regsubsets(price_numeric~., data = data1, nvmax = 5,
                     method = "seqrep")
summary(models)

Best_Subset <-
  regsubsets(price_numeric~.,
             data = data1,
             nbest = 1,      # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")

summary_best_subset <- summary(Best_Subset)
as.data.frame(summary_best_subset$outmat)


num_predictors <-which.max(summary_best_subset$adjr2)
num_predictors
summary_best_subset$which[num_predictors,]




