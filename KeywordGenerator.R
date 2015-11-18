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

findFreqTerms(DTMGood, 5)
findFreqTerms(DTMBad, 5)
KeywordsGood <- sort(round(apply(DTMGood, MARGIN = 2, FUN = mean),2), decreasing = T)
KeywordsBad <- sort(round(apply(DTMBad, MARGIN = 2, FUN = mean),2), decreasing = T)
print("Keywords associated with good reviews:")
print(KeywordsGood[1:20])
print("Keywords associated with bad reviews:")
print(KeywordsBad[1:20])


#To follow wordcloud's recipe
mGood <- as.matrix(DTMGood)
mBad <- as.matrix(DTMBad)

vGood <- sort(colSums(mGood), decreasing = T)
vBad <- sort(colSums(mBad), decreasing = T)
dGood <- data.frame(word = names(vGood), freq=vGood)
dBad <- data.frame(word = names(vBad), freq=vBad)


wordcloud(dGood$word, dGood$freq, scale=c(2.5,0.5), 
          max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8, 'Dark2'))


wordcloud(dBad$word, dBad$freq, scale=c(2.5,0.5), 
          max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8, 'Dark2'))

