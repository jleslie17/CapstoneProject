##File to be called to generate Keywords

if(BID %in% WorstRevsGrouped$business_id){
        QueryDB <- WorstRevsGrouped[WorstRevsGrouped$business_id == BID,]
} else {
        QueryDB <- BestRevsGrouped[BestRevsGrouped$business_id == BID,]
}


QueryDBGood <- QueryDB[QueryDB$stars > StarsThresh,]
QueryDBBad <- QueryDB[QueryDB$stars <= StarsThresh,]

#Function to make a corpus
GetCorpus <- function(inputDB){
        CorpusTemp <- VCorpus(VectorSource((inputDB$text)))
        CorpusTemp <- tm_map(CorpusTemp, stripWhitespace)
        CorpusTemp <- tm_map(CorpusTemp, content_transformer(tolower))
        CorpusTemp <- tm_map(CorpusTemp, removeWords, stopwords("english"))
        CorpusTemp <- tm_map(CorpusTemp, stemDocument) #Maybe don't want to do this?
        return(CorpusTemp)
}

CorpusBad <- GetCorpus(QueryDBBad)
CorpusGood <- GetCorpus(QueryDBGood)

##Function to build DTM
getDTM <- function(inputCorp){
        dtmTemp <- DocumentTermMatrix(inputCorp)
        dtmTemp <- removeSparseTerms(dtmTemp,0.9)
        return(dtmTemp)
}

DTMGood <- getDTM(CorpusGood)
DTMBad <- getDTM(CorpusBad)
KeywordsGood <- sort(round(apply(DTMGood, MARGIN = 2, FUN = mean),2), decreasing = T)
KeyWordsBad <- sort(round(apply(DTMBad, MARGIN = 2, FUN = mean),2), decreasing = T)
KeywordsGood
KeyWordsBad
