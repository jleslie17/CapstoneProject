library(jsonlite)
library(tm)
library(dplyr)
library(zoo)
library(ggplot2)
##Can start from here once data saved as rds files in working directory

##Reload the data
BusData <- readRDS("businessData.rds")
ReviewData <- readRDS("reviewData.rds")

##Grab all businesses that have "Restaurants" in their category
Restaurants <- BusData[grepl("Restaurants", BusData$categories),]
##Grabbing all reviews of the restaurant businesses
RestRevs <- ReviewData[ReviewData$business_id %in% 
                               Restaurants$business_id,]
##Selecting a shortlist of restaurants that have 50 or more reviews
Threshold <- 100
RestsHiCount <- Restaurants[Restaurants$review_count >= Threshold,]
##Gather all the reviews for those restaurants
RestRevsHiCount <- RestRevs[RestRevs$business_id %in%
                                    RestsHiCount$business_id,]

##Will have BID defined here.
BID <- "27ADmOieSUZfbiU15als7w" #This is where I put in the business ID of interest
StarsThresh <- 3.5

BID <- "2WHP5nhS1rFszfRBKe6fWQ"

source("getMonthlyAverages.R")
source("KeywordGenerator.R")

