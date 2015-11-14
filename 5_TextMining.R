##Text Mining!
library(SnowballC)

CorpusBest <- VCorpus(VectorSource(BestRevsGathered$Texts))
CorpusWorst <- VCorpus(VectorSource(WorstRevsGathered$Texts))



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
tdmFreq <- findFreqTerms(tdm, 50, Inf) #Character vector
str(tdm)
tdm[, "also"]
sort(tdmFreq)


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
x <- as.numeric(SushiNames)
y <- as.numeric(testSushi)
plot(x,y) 
abline(lm(y~x))
lm(y~x)$coef[2]
        



inspect(tdm[tdmFreq[1],])
inspect(tdm['also',])
inspect(tdm['sushi',])

y=Terms(tdm[tdmFreq[1],])
x=Docs(tdm)
plot(, y)

Docs(tdm)







m3 <- as.matrix(tdm$dimnames)

tdm$dimnames$Terms[1:10, tdmFreq]

tdm$dimnames$Docs == "55"
tdm[tdm$dimnames$Docs == "55"]
findFreqTerms(tdm[tdm$dimnames$Docs == "55"], 20)
tdm[6]

dtm <- DocumentTermMatrix(CorpusTestTrans)
dtm2 <- removeSparseTerms(dtm, 0.4)
inspect(dtm2)


findFreqTerms(dtm, 20, 100)
CorpusTestTrans[[2]]


dtm$dimnames$Docs
findFreqTerms(dtm[dtm$dimnames$Docs == "1"], 20, 100)
dtm[1]
str(dtm)
inspect(removeSparseTerms(dtm, 0.4))
termFreq()
findFreqTerms()
dtm$dimnames$Docs

m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = T)
head(v, 10)
v2 <- sort(m, decreasing = T)
head(v2, 10)

findFreqFunction <- function(x) {
        findFreqTerms(x, 20, Inf)
}

tdm[1:10, findFreqFunction]

findAssocs(tdm, "qualiti", 0.5)
inspect(removeSparseTerms(tdm, 0.4))
DictDTMTest <- inspect(DocumentTermMatrix(CorpusTestTrans, 
                           list(dictionary = c('sushi', 'slow','rude','qualiti'))))
str(DictDTMTest)
stopwords('en')
test <- as.matrix(DictDTMTest)
test[1,]
