
require(reshape2)
require(ggplot2)
setwd('C:/etc/Projects/Data/_Ongoing/MovieTaglines')

df<-read.csv('C:/Users/eeshanmalhotra/Desktop/words.csv',stringsAsFactors=F)

m<-as.matrix(df[,2:9])
row.names(m)<-df[,1]

x1<-melt(t(m))
x1$Var2<-as.character(x1$Var2)

names(x1)=c("Decade","Words","colour")

x1$colour=factor(as.numeric(x1$colour>0.5)*2)

levels(x1$colour)=c("Not in Top 10","In Top 10")
x1$Words<-factor(x1$Words, levels=unique(rev(x1$Words)))
x1$Decade<-factor(x1$Decade, levels=unique(x1$Decade))

p<-qplot(Decade, Words, fill=colour, data=x1, geom='tile')
p
p+scale_fill_manual(name = "This is my title", values = c("#EEEEEE", "#00BFC4"), labels = c("0" = "Foo", "1" = "Bar"))
p+scale_fill_manual(name = "This is my title", values = c("#E5E5E5", "#F8766D"), labels = c("0" = "Foo", "1" = "Bar"))