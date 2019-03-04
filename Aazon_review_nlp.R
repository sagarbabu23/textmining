install.packages("tm")
install.packages("slam")
install.packages("topicmodels")
library(NLP)
library(tm,slam)
library(topicmodels)
library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/OnePlus-Mirror-Black-64GB-Storage/product-reviews/B0756Z43QS/ref=cm_cr_getr_d_paging_btm_1?showViewpoints=1&pageNumber=1"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"onplus1.txt",row.names = F)
getwd()## Using While loop to get all reviews without using page number ############## 
#### Simple Example ##################################################
samp_url <- "https://www.amazon.in/OnePlus-Mirror-Black-64GB-Storage/product-reviews/B0756Z43QS/ref=cm_cr_getr_d_paging_btm_1?showViewpoints=1&pageNumber"
i=1
p=1
oneplus <- NULL
while(p>0){
  t_url <- read_html(as.character(paste(samp_url,i,sep="=")))
  rev <- t_url %>%
    html_nodes(".review-text") %>%
    html_text()
  oneplus<- c(oneplus,rev)
  i <- i+1
  p=length(rev)
  amazon_reviews <- c(oneplus,rev)
}

install.packages("wordcloud")
library(wordcloud)



# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)
corpus = Corpus(VectorSource(amazon_reviews))

wordcloud(corpus,max.words = 200,random.color = TRUE,random.order=FALSE)


getwd()
setwd("D://assignments//text mining")
