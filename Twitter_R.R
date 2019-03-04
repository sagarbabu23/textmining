install.packages("twitteR")

library("twitteR")
install.packages("ROAuth")
library("ROAuth")
install.packages("RCurl")
library(RCurl)



cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata"install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('msdhoni', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()
# 
handleTweets <- searchTwitter('DataScience', n = 10000)
# handleTweetsDF <- twListToDF(handleTweets)
# dim(handleTweetsDF)
# View(handleTweetsDF)
# #handleTweetsMessages <- unique(handleTweetsDF$text)
# #handleTweetsMessages <- as.data.frame(handleTweetsMessages)
# #write.csv(handleTweetsDF, "TefalHandleTweets.csv")
# 
library(rtweet)

# CLEANING TWEETS

TweetsDF$text=gsub("&amp", "", TweetsDF$text)
TweetsDF$text = gsub("&amp", "", TweetsDF$text)
TweetsDF$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", TweetsDF$text)
TweetsDF$text = gsub("@\\w+", "", TweetsDF$text)
TweetsDF$text = gsub("[[:punct:]]", "", TweetsDF$text)
TweetsDF$text = gsub("[[:digit:]]", "", TweetsDF$text)
TweetsDF$text = gsub("http\\w+", "", TweetsDF$text)
TweetsDF$text = gsub("[ \t]{2,}", "", TweetsDF$text)
TweetsDF$text = gsub("^\\s+|\\s+$", "", TweetsDF$text)

TweetsDF$text <- iconv(TweetsDF$text, "UTF-8", "ASCII", sub="")


# Emotions for each tweet using NRC dictionary
install.packages("RCurl")
install.packages("httr")
install.packages("syuzhet")
install.packages("NLP")
library(RCurl)
library(httr)
library(syuzhet)
library(NLP)

emotions <- get_nrc_sentiment(TweetsDF$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

install.packages("plotly")
library(plotly)
p <- plot_ly(emo_sum, x=emotions, y=emo_sum, type="bar", color=emotions) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag")
api_create(p,filename="Sentimentanalysis")


# Create comparison word cloud data
install.packages("tm")
install.packages("wordcloud")
install.packages("ggplot2", "reshape2", "plyr", "languageR",
                 "lme4", "psych")
install.packages("tidytext")
library(plyr)
library(tm)
library(wordcloud)
library(tidytext)
wordcloud_tweet = c(
  paste(TweetsDF$text[emotions$anger > 0], collapse=" "),
  paste(TweetsDF$text[emotions$anticipation > 0], collapse=" "),
  paste(TweetsDF$text[emotions$disgust > 0], collapse=" "),
  paste(TweetsDF$text[emotions$fear > 0], collapse=" "),
  paste(TweetsDF$text[emotions$joy > 0], collapse=" "),
  paste(TweetsDF$text[emotions$sadness > 0], collapse=" "),
  paste(TweetsDF$text[emotions$surprise > 0], collapse=" "),
  paste(TweetsDF$text[emotions$trust > 0], collapse=" ")
)

# create corpus


corpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

wordcloud(corpus,max.words = 200,random.color = TRUE,random.order=FALSE)


# create document term matrix

tdm = TermDocumentMatrix(corpus)
tdm
dim(tdm)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]
tdmnew
dim(tdmnew)

setwd("D://assignments//text mining")
