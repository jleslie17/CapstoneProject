##Text Mining!
library(SnowballC)

CorpusBest <- VCorpus(VectorSource(BestRevsGathered$Texts))
CorpusWorst <- VCorpus(VectorSource(WorstRevsGathered$Texts))

CorpusWorstTrans <- tm_map(CorpusWorst, stripWhitespace)
CorpusWorstTrans <- tm_map(CorpusWorstTrans, content_transformer(tolower))
CorpusWorstTrans <- tm_map(CorpusWorstTrans, removeWords, stopwords("english"))
CorpusWorstTrans <- tm_map(CorpusWorstTrans, stemDocument)

tdmWorst <- TermDocumentMatrix(CorpusWorstTrans)
tdmWorstFreq <- findFreqTerms(tdmWorst, 50, Inf) #Character vector

##Function to extract keywords from a corpus
FindKeyWords <- function(inputCorp){
        dtmTemp <- DocumentTermMatrix(inputCorp)
        dtmTemp <- removeSparseTerms(dtmTemp,0.4)
        dtmFreqTemp <- findFreqTerms(dtmTemp, 3)
        return(dtmFreqTemp)
}

##This code produces a corpus for the business querry
BID <- "27ADmOieSUZfbiU15als7w" #This is where I put in the business ID of interest
QueryDB <- WorstRevsGathered[WorstRevsGathered$business_id == BID,]
##Loop to get the keywords from each business in the worst list
for(i in unique(QueryDB)){
        CorpusTemp <- VCorpus(VectorSource(QueryDB$Texts))
        CorpusTemp <- tm_map(CorpusTemp, stripWhitespace)
        CorpusTemp <- tm_map(CorpusTemp, content_transformer(tolower))
        CorpusTemp <- tm_map(CorpusTemp, removeWords, stopwords("english"))
        CorpusTemp <- tm_map(CorpusTemp, stemDocument)
        QueryCorpus <- CorpusTemp
}

for(i in 1:length(QueryCorpus)){
        SubCorpus <- QueryCorpus[i]
        Keywords <- FindKeyWords(SubCorpus)
        print(i)
        print(Keywords)
}

head(WorstRevsGrouped)


test <- QueryCorpus[1]
inspect(QueryCorpus[1])
QueryCorpus[[49]]$content
FindKeyWords(QueryCorpus[1])







tdm <- TermDocumentMatrix(CorpusTestTrans)
tdmSparseRem <- removeSparseTerms(tdm,0.4)
tdmFreq <- findFreqTerms(tdmSparseRem, 5, Inf) #Character vector

###This is how I'll do it!
test <- inspect(tdm[tdmFreq, ])
test['sushi',]
row.names(test)
for(i in row.names(test)) {
        print(i)
}
testSushi <- test['sushi',]
SushiNames <- names(testSushi)
summary(testSushi)

test <- inspect(tdm[tdmFreq, ])
x <- as.numeric(SushiNames)
plot(x,y,type = 'n')
for(i in tdmFreq){ #This is the character vector of keywords
        termTrend <- test[i,]
        y <- as.numeric(termTrend)
        abline(lm(y~x))
        print(lm(y~x)$coef[2])
}









WorstWordsDF[1,1]
unique(KeywordListBest)

TMTestVector <- WorstRevsGathered[WorstRevsGathered$business_id == 
                                          "27ADmOieSUZfbiU15als7w",]
CorpusTest <- VCorpus(VectorSource(TMTestVector$Texts))
inspect(CorpusTest[1]) #each index is a month's reviews
meta(CorpusTest[[2]], "id") #id for each month
writeLines(as.character(CorpusTest[[2]])) #getting the words from month 2
TMTestVector$Texts[1:5]
lapply(CorpusTest[[2]], as.character)
writeLines(as.character(CorpusTest[[1]]))

#Transformations Testing
CorpusTestTrans <- tm_map(CorpusTest, stripWhitespace)
CorpusTestTrans <- tm_map(CorpusTestTrans, content_transformer(tolower))
CorpusTestTrans <- tm_map(CorpusTestTrans, removeWords, stopwords("english"))
CorpusTestTrans <- tm_map(CorpusTestTrans, stemDocument)

#could also perform synonym analysis and remove sparse terms. See ref

#TermDocumentMatrix
tdm <- TermDocumentMatrix(CorpusTestTrans)
tdmSparseRem <- removeSparseTerms(tdm,0.4)
tdmFreq <- findFreqTerms(tdmSparseRem, 5, Inf) #Character vector
tdmFreq
Keywords <- list()
count <- 0
FindKeyWords <- function(inputCorp){
        dtmTemp <- DocumentTermMatrix(inputCorp)
        dtmTemp <- removeSparseTerms(dtmTemp,0.4)
        dtmFreqTemp <- findFreqTerms(dtmTemp, 5)
        return(dtmFreqTemp)
}
FindKeyWords(CorpusTestTrans)
Keywords <- c(Keywords, tdmFreq)
Keywords <- c
testList <- list(tdm, tdmSparseRem)
testList <- append(testList,list(tdm))


###This is how I'll do it!
test <- inspect(tdm[tdmFreq, ])
test['sushi',]
row.names(test)
for(i in row.names(test)) {
        print(i)
}
testSushi <- test['sushi',]
SushiNames <- names(testSushi)
summary(testSushi)

test <- inspect(tdm[tdmFreq, ])
x <- as.numeric(SushiNames)
plot(x,y,type = 'n')
for(i in tdmFreq){
        termTrend <- test[i,]
        y <- as.numeric(termTrend)
        abline(lm(y~x))
        print(lm(y~x)$coef[2])
}

##Find associations
word = "disappoint"
dictionary = c("poor", "servic","rude")
findAssocs(tdm, dictionary, 0.5)
