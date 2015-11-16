##File to be called to generate Keywords
require(tm)
require(wordcloud)


QueryDB <- BIDStarsDates
QueryDBGood <- QueryDB[QueryDB$stars > StarsThresh,]
QueryDBBad <- QueryDB[QueryDB$stars <= StarsThresh,]

#Function to make a corpus
GetCorpus <- function(inputDB){
        CorpusTemp <- VCorpus(VectorSource((inputDB$text)))
        CorpusTemp <- tm_map(CorpusTemp, stripWhitespace)
        CorpusTemp <- tm_map(CorpusTemp, content_transformer(tolower))
        CorpusTemp <- tm_map(CorpusTemp, removeWords, stopwords("english"))
        CorpusTemp <- tm_map(CorpusTemp, stemDocument) #Maybe don't want to do this?
        CorpusTemp <- tm_map(CorpusTemp, removePunctuation)
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


findFreqTerms(DTMGood, 100)
findFreqTerms(DTMBad,100)
KeywordsGood <- sort(round(apply(DTMGood, MARGIN = 2, FUN = mean),2), decreasing = T)
KeywordsBad <- sort(round(apply(DTMBad, MARGIN = 2, FUN = mean),2), decreasing = T)
print("Keywords associated with good reviews:")
print(KeywordsGood)
print("Keywords associated with bad reviews:")
print(KeywordsBad)
print(KeywordsBad[1:20])


wordcloud(CorpusGood, scale=c(5,0.5), 
          max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8, 'Dark2'))


wordcloud(CorpusBad, scale=c(5,0.5), 
          max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8, 'Dark2'))

intersect(KeywordsGood, KeywordsBad)
