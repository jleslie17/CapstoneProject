##Text Mining!
library(SnowballC)

CorpusBest <- VCorpus(VectorSource(BestRevsGathered$Texts))
CorpusWorst <- VCorpus(VectorSource(WorstRevsGathered$Texts))



TMTestVector <- WorstRevsGathered[WorstRevsGathered$business_id == 
                                          "27ADmOieSUZfbiU15als7w",]
CorpusTest <- VCorpus(VectorSource(TMTestVector$Texts))
inspect(CorpusTest[2])
meta(CorpusTest[[2]], "id")
writeLines(as.character(CorpusTest[[2]]))
TMTestVector$Texts[1:5]
lapply(CorpusTest[[2]], as.character)
writeLines(as.character(CorpusTest[[1]]))


#Transformations Testing
CorpusTestTrans <- tm_map(CorpusTest, stripWhitespace)
CorpusTestTrans <- tm_map(CorpusTestTrans, content_transformer(tolower))
CorpusTestTrans <- tm_map(CorpusTestTrans, removeWords, stopwords("english"))
CorpusTestTrans <- tm_map(CorpusTestTrans, stemDocument)

#could also perform synonym analysis. See ref

#TermDocumentMatrix
tdm <- TermDocumentMatrix(CorpusTestTrans)
findFreqTerms(tdm, 10)
findAssocs(tdm, "slow", 0.5)
inspect(removeSparseTerms(tdm, 0.4))
inspect(DocumentTermMatrix(CorpusTestTrans, 
                           list(dictionary = c('sushi', 'slow','rude'))))

