####################################
##### Highest Divergence Terms #####
####################################


library(tm)
library(stringr)

setwd('C:/etc/Projects/Data/MovieTaglines')

#Taglines Data
dfTaglines <- read.table('data/taglines.tsv', header=F, sep="\t",quote="",comment.char="")
colnames(dfTaglines)<-c("title","tagline")
dfTaglines[,2] <- gsub("'","",dfTaglines[,2])
dfTaglines[,2] <- gsub("[[:punct:]]"," ",dfTaglines[,2])

dim(dfTaglines)



#Ratings Data
dfRatings<-read.table('data/ratings.list',header=T,sep="\t",colClasses="character",quote="",comment.char="")
dfRatings$Votes<-as.numeric(dfRatings$Votes)
dfRatings$Rank<-as.numeric(dfRatings$Rank)

colnames(dfRatings)<-tolower(colnames(dfRatings))
dim(dfRatings)


#Merge
dff<-merge(dfTaglines,dfRatings,by="title")
dim(dff)
 #A good proportion of movies are left after the merge without any need for fuzzy text matching techniques. Let's just go ahead with this.
dff$year<-as.numeric(substr(str_extract(dff$title," \\(([0-9]{4})"),3,6))


#Text Analysis
set.seed(245)

sampleSize<-10000 #For this anlysis, you need a higher number. Below 10000 is not a good idea

if(sampleSize>nrow(dff)) { 
	samplePoints <- 1:nrow(dff)
} else {
	samplePoints <- sort(sample(1:nrow(dff),sampleSize))
}


taglines.corpus<-Corpus(DataframeSource(data.frame(dff[samplePoints,"tagline"])))
taglines.corpus <- tm_map(taglines.corpus, removePunctuation)
taglines.corpus <- tm_map(taglines.corpus, tolower)
taglines.corpus <- tm_map(taglines.corpus, function(x) removeWords(x, c(stopwords("english"),"original","print","ad","poster","mostly","caps","lobby","cardall", "usa","card") ))
taglines.corpus <- tm_map(taglines.corpus, stripWhitespace)

tdm <- TermDocumentMatrix(taglines.corpus)
tdm <- removeSparseTerms(tdm,1-(10/sampleSize))

mTerms <- as.matrix(tdm)
dfTerms <- as.data.frame(t(mTerms>0))

termFreq <- colSums(dfTerms)
dfTerms <-dfTerms[,termFreq>=10] # only of sparse-term-removal and this is required.

dfTerms$movietitle <- dff[samplePoints,1]
dfTerms$movierating <- dff[samplePoints,4]


Divergence<-function(x,y){
	a <- mean(x)-mean(y)
	b <- (sd(x) + sd(y))/2
	sign(a)* (a^2)/b  # the sign is not part of the divergence value, but is added to aid my segregation of 'good' and 'bad' words
}


n<-ncol(dfTerms)-2
divergenceValues<-apply(dfTerms[,1:n],2,function(x) Divergence(dfTerms$movierating[x==T],dfTerms$movierating[x==F]))

divergenceValues<-sort(divergenceValues,decreasing=T)
head(divergenceValues,10)
rev(tail(divergenceValues,10))


