# Twitter Data Mining Project
# 9.27.2017
# Nichole Freeman

# searched for key words and topics
# created a word cloud to give a visual of main words 


install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud") 

require(twitteR)
require(RCurl)
require(tm)  # most popular text mining 
require(wordcloud)  # creates text visuals 

consumer_key <- "YjZRiZJ3k7F2sJSARKkWI2S3e"
consumer_secret <- "zsxP8xTRdIniwsuOqtVrIlJxrvyylaqjBItdODzdXRmskFkwIN"
access_token <- "2363189640-eE7cgEI0oGWrocHnqTluZnf8Z8Fo8hQKFRL6obP"
access_secret <- "n6oFEKkWf5E3pA409OZvR5XinyzwkO0IJHAJa4opeV4fA"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# gives us authorization to get into twitter

MUFC_tweets <- searchTwitter("MUFC", n = 1, lang = "en")
# searched manchester united for tweets 
# comes out in a list

#going to search tweets on the hurricane in PuertoRico

HPR <- searchTwitter('hurricane+Puerto+Rico', lang ="en", n=500, resultType ="recent")
# HPR is now a list of 10 tweets 
# lets turn this is into characters so we can use them 

HPR_text <- sapply(HPR, function(x) x$getText())
str(HPR_text) # shows the structure ... says character vector of  10 elements 

HPR_corpus <- Corpus(VectorSource(HPR_text)) # using TM package create out of vectors
inspect(HPR_corpus[1]) # can look at first vector 

# going to clean up text so we can create a good word cloud
HPR_clean <- tm_map(HPR_corpus, removePunctuation)
HPR_clean <- tm_map(HPR_clean, content_transformer(tolower))
HPR_clean <- tm_map(HPR_clean, removeWords, stopwords("english")) 
# takes out uninteresting words
HPR_clean <- tm_map(HPR_clean, removeNumbers)
HPR_clean <- tm_map(HPR_clean, stripWhitespace)
HPR_clean <- tm_map(HPR_clean, removeWords, c("hurricane", "puerto", "rico"))
# those words are obviously going to come up, so lets not have them in the cloud

# Alright! lets do a simple word cloud 
wordcloud(HPR_clean)  # tada!
# random.order = F will have bigger words gravetate towards the center
# scale is max and min font size scale = c(5, 2.5)
wordcloud(HPR_clean, random.order = F, max.words = 100, colors=rainbow(50))

