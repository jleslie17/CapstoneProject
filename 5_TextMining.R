library(jsonlite)
library(tm)
library(dplyr)
library(zoo)
library(ggplot2)
library(SnowballC)


StarsThresh <- 3.5
BID <- "_viHcOY9w_CitP4oHpZYdg" #This is where I put in the business ID of interest
source("KeywordGenerator.R")
KeywordsGood
KeyWordsBad


head(unique(WorstRevsGathered$business_id))
head(unique(BestRevsGathered$business_id))

head(BestRevsGathered[BestRevsGathered$business_id == "_4_Qn9WZVwofeaqjgQG2Zw",])

