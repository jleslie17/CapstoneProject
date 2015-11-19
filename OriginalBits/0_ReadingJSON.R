library(jsonlite)
library(tm)
library(readr)


PATH <- "yelp_dataset_challenge_academic_dataset"
BusData <- stream_in(file(paste(PATH, "/",
                                "yelp_academic_dataset_business.json",sep = "")))
UserData <- stream_in(file(paste(PATH, "/", 
                                 "yelp_academic_dataset_user.json", sep = "")))

reviewData <- stream_in(file(paste(PATH, "/", 
                                   "yelp_academic_dataset_review.json", sep = "")))
tipData <- stream_in(file(paste(PATH, "/",
                                "yelp_academic_dataset_tip.json", sep = "")))
CheckinData <- stream_in(file(paste(PATH, "/",
                                    "yelp_academic_dataset_checkin.json",
                                    sep = "")))

saveRDS(reviewData, "reviewData.rds")
saveRDS(BusData, "businessData.rds")
saveRDS(tipData, "tipData.rds")
saveRDS(UserData, "userData.rds")
saveRDS(CheckinData, "checkinData.rds")

