
#####################################
##### Frequent terms by decade ######
#####################################

# This same structure could use segmentation by  any criterion, really.
# Year just seems interesting at this point of time
# Maybe genre


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

#One of the reasons for not removing the years from the titles in the raw data was to be able to extract this here:
dfTaglines$year<-as.numeric(substr(str_extract(dfTaglines$title," \\(([0-9]{4})"),3,6))


dim(dfTaglines)



#Segmenting by decade and doing the frequent word analysis

#Functions which are called for each decade/segment
	PlotWordCloud<-function(terms,freqs,saveToFile=F,filename="out/wordcloud.png"){
		
		pal <- brewer.pal(9, "Blues")
		pal <- pal[-(1:2)]
		if(saveToFile){ png(filename, width=1000,height=1000,res=100)  }
		wordcloud(terms,freqs, min.freq=3,max.words=100, random.order=T, rot.per=.15, colors=pal,scale=c(5,0.5))
		if(saveToFile) { dev.off() }
	}
	
	
	
	TopWords<- function(df,sampleSize=10000,...){
 		set.seed(414)
		
		if(sampleSize>nrow(df)) { 
			samplePoints <- 1:nrow(df)
		} else {
			samplePoints <- sample(1:nrow(df),sampleSize)
		}
		
		
		taglines.corpus<-Corpus(DataframeSource(data.frame(df[samplePoints,"tagline"])))
		taglines.corpus <- tm_map(taglines.corpus, removePunctuation)
		taglines.corpus <- tm_map(taglines.corpus, tolower)
		taglines.corpus <- tm_map(taglines.corpus, function(x) removeWords(x, c(stopwords("english"),"original","print","ad","poster","mostly","caps","lobby","cardall", "usa","card") ))
		taglines.corpus <- tm_map(taglines.corpus, stripWhitespace)
		
		tdm <- TermDocumentMatrix(taglines.corpus)
		
		mTerms <- as.matrix(tdm)
		termFreq <- sort(rowSums(mTerms>0),decreasing=TRUE)
		dfTermFreq <- data.frame(term = names(termFreq),freq=termFreq)
		row.names(dfTermFreq) <- 1:nrow(dfTermFreq)
		
		PlotWordCloud(dfTermFreq$term,dfTermFreq$freq,...)
		
		dfTermFreq
		
	}



dfModern<-dfTaglines[dfTaglines$year>=1940,]
lower<-seq(1940,2010,10)
upper<-lower+10

lTopWordsByDecade<-lapply(1:length(lower),function(i){
	dfDecade <- dfModern[dfModern$year>=lower[i] & dfModern$year<upper[i],]
	twDecade <- TopWords(dfDecade,saveToFile=T,filename=paste0("out/wordcloud_",lower[i],"_",upper[i],".png"))
	colnames(twDecade)<-paste0(colnames(twDecade),".",lower[i],".",upper[i])
	twDecade
})

lTop10WordsByDecade<-lapply(lTopWordsByDecade, function(x) x[1:10,])
dfTop10Words<-do.call(cbind,lTop10WordsByDecade)

write.csv(dfTop10Words,"out/Top10WordsbyDecade.csv")


