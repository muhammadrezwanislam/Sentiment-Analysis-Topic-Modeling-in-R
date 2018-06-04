setwd("C:/Twitter Mining")
rm(list = ls())
#Solve the character encoding problem
library(readr)
options(stringsAsFactors = FALSE)
mydata<-read_csv("english_tweets.csv",
                 locale = locale(encoding = "UTF-8"))
View(mydata)

#remove non-ascii character

tweet.raw<-(iconv(mydata$description[1:50000], "latin1", "ASCII", sub=""))

library(sentiment)
library(tm)
library(NLP)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)


clean_tweet = gsub("&amp", "", tweet.raw)
#remove retweet entities 
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
#remove at people 
clean_tweet = gsub("@\\w+", "", clean_tweet)
#remove punctuation
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
#remove numbers 
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
#remove html links 
clean_tweet = gsub("http\\w+", "", clean_tweet)
#remove unnecessary space 
clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)




library(syuzhet)

a<-get_nrc_sentiment(clean_tweet[1:50000])
write.csv(a, "sentimentscore.csv")


#draw barplot using ggplot2 library 
Sentimentscore <- data.frame(colSums(a[,]))
names (Sentimentscore) <- "Score"
Sentimentscore <-cbind("sentiment"=rownames(Sentimentscore), Sentimentscore)
rownames(Sentimentscore)<- NULL
ggplot(data=Sentimentscore, aes(x=Sentimentscore$sentiment, y=Sentimentscore$Score))+geom_bar(aes(fill=sentiment),stat='identity')+xlab("Sentiment")+ylab("score")+ggtitle("total sentiment score based on tweets")

write.csv(Sentimentscore, "combinedsentimentscore.csv")
