library(jsonlite)
library(tm)
library(dplyr)
library(zoo)
library(ggplot2)
library(SnowballC)



##This code produces a corpus for the business querry
StarsThresh <- 3
BID <- "27ADmOieSUZfbiU15als7w" #This is where I put in the business ID of interest
QueryDB <- WorstRevsGrouped[WorstRevsGrouped$business_id == BID,]
QueryDBGood <- QueryDB[QueryDB$stars > StarsThresh,]
QueryDBBad <- QueryDB[QueryDB$stars <= StarsThresh,]
##Loop to get the keywords from each business in the worst list
for(i in 1:nrow(QueryDB)){
        CorpusTemp <- VCorpus(VectorSource(QueryDB$text))
        CorpusTemp <- tm_map(CorpusTemp, stripWhitespace)
        CorpusTemp <- tm_map(CorpusTemp, content_transformer(tolower))
        CorpusTemp <- tm_map(CorpusTemp, removeWords, stopwords("english"))
        CorpusTemp <- tm_map(CorpusTemp, stemDocument)
        QueryCorpus <- CorpusTemp
        print(i)
        print(QueryDB$stars[i])
}

GetCorpus <- function(inputDB){
        CorpusTemp <- VCorpus(VectorSource((inputDB$text)))
        CorpusTemp <- tm_map(CorpusTemp, stripWhitespace)
        CorpusTemp <- tm_map(CorpusTemp, content_transformer(tolower))
        CorpusTemp <- tm_map(CorpusTemp, removeWords, stopwords("english"))
        CorpusTemp <- tm_map(CorpusTemp, stemDocument)
        return(CorpusTemp)
}

CorpusBad <- GetCorpus(QueryDBBad)
CorpusGood <- GetCorpus(QueryDBGood)
KeyWordsBad <- FindKeyWords(CorpusBad)
KeyWordsGood <- FindKeyWords(CorpusGood)

##Function to extract keywords from a corpus
FindKeyWords <- function(inputCorp){
        dtmTemp <- DocumentTermMatrix(inputCorp)
        dtmTemp <- removeSparseTerms(dtmTemp,0.6)
        dtmFreqTemp <- findFreqTerms(dtmTemp, 3)
        return(dtmFreqTemp)
}


for(i in 1:length(QueryCorpus)){
        SubCorpus <- QueryCorpus[i]
        Keywords <- FindKeyWords(SubCorpus)
        print(i)
        print(Keywords)
}

head(WorstRevsGrouped)
range(WorstRevsGrouped$stars)
