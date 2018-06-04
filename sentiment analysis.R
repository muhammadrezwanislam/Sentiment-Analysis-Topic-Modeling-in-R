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

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
clean_tweet = sapply(clean_tweet, try.error)

head(clean_tweet)

# classify emotion
class_emo = classify_emotion(clean_tweet, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(clean_tweet, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=clean_tweet, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

head(sent_df)
# plot distribution of emotions
library(ggplot2)
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets",title = "Sentiment Analysis of Tweets about Starbucks\n(classification by emotion)")



library(syuzhet)

a<-get_nrc_sentiment(clean_tweet[1:50000])
write.csv(a, "sentimentscore.csv")

#draw barplot using default r system
#barplot(100*colSums(a)/sum(a),, las = 2, col=rainbow(10), ylab = 'count', main = '% of sentiment')

#draw barplot using ggplot2 library 
Sentimentscore <- data.frame(colSums(a[,]))
names (Sentimentscore) <- "Score"
Sentimentscore <-cbind("sentiment"=rownames(Sentimentscore), Sentimentscore)
rownames(Sentimentscore)<- NULL
ggplot(data=Sentimentscore, aes(x=Sentimentscore$sentiment, y=Sentimentscore$Score))+geom_bar(aes(fill=sentiment),stat='identity')+xlab("Sentiment")+ylab("score")+ggtitle("total sentiment score based on tweets")

write.csv(Sentimentscore, "combinedsentimentscore.csv")
