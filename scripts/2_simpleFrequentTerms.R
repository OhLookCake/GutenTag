
#####################################
### Simple Frequent terms detector###
#####################################

library(tm)
library(wordcloud)
library(RColorBrewer)
library(stringr)

setwd('C:/etc/Projects/Data/MovieTaglines')

#Taglines Data
dfTaglines <- read.table('data/taglines.tsv', header=F, sep="\t",quote="",comment.char="")
colnames(dfTaglines)<-c("title","tagline")
dfTaglines[,2] <- gsub("'","",dfTaglines[,2])
dfTaglines[,2] <- gsub("[[:punct:]]"," ",dfTaglines[,2])

dim(dfTaglines)





#Text Analysis
set.seed(108)
sampleSize <- 10000 
# Adjust this number depending on how many movies you analyze. Going very high may lead to long computation times.
# Things actually stabilize around 2000 movies
samplePoints <- sample(1:nrow(dfTaglines),sampleSize)

taglines.corpus<-Corpus(DataframeSource(data.frame(dfTaglines[samplePoints,2])))
taglines.corpus <- tm_map(taglines.corpus, removePunctuation)
taglines.corpus <- tm_map(taglines.corpus, tolower)
taglines.corpus <- tm_map(taglines.corpus, function(x) removeWords(x, c(stopwords("english"),"original","print","ad","poster","mostly","caps","lobby","cardall", "usa","card") ))
taglines.corpus <- tm_map(taglines.corpus, stripWhitespace)

tdm <- TermDocumentMatrix(taglines.corpus)

mTerms <- as.matrix(tdm)
termFreq <- sort(rowSums(mTerms>0),decreasing=TRUE)
dfTermFreq <- data.frame(term = names(termFreq),freq=termFreq)
row.names(dfTermFreq) <- 1:nrow(dfTermFreq)

head(dfTermFreq,10)

pal <- brewer.pal(9, "YlOrRd")
pal <- pal[-(1:2)]
png("out/wordcloud_global.png", width=800,height=800,res=150) 
wordcloud(dfTermFreq$term,dfTermFreq$freq, min.freq=3,max.words=100, random.order=T, rot.per=.25, colors=pal,use.r.layout=T,scale=c(5,0.5))
dev.off()




