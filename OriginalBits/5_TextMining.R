library(jsonlite)
library(tm)
library(dplyr)
library(zoo)
library(ggplot2)
library(SnowballC)


StarsThresh <- 3.5
#This is where I put in the business ID of interest
BID <- "_viHcOY9w_CitP4oHpZYdg" 
source("KeywordGenerator.R")
KeywordsGood
KeyWordsBad

length(intersect(KeywordsGood, KeyWordsBad))



head(unique(WorstRevsGathered$business_id))
head(unique(BestRevsGathered$business_id))

head(BestRevsGathered[BestRevsGathered$business_id == "_4_Qn9WZVwofeaqjgQG2Zw",])

