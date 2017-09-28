# Twitter Sentiment Analysis
# 9.27.2017
# Nichole Freeman

# same initial code from twitter_search for initial access and packages
# This is a script on sentiment analysis

# references used 
# trial runs from Edureka youtube videos as reference 
# https://www.youtube.com/watch?v=-JW6_kcHDj4
# http://tidytextmining.com/sentiment.html#the-sentiments-dataset
# https://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/


install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud") 
install.packages("purrr")
install.packages("tidytext") 
install.packages("tidyverse")



require(twitteR)
require(RCurl)
require(tm)  # most popular text mining 
require(wordcloud)  # creates text visuals 
require(dplyr)
require(plyr)
require(stringr)
require(tidytext) # has a large list of sentiment lexicons in data set 
require(tidyverse)

consumer_key <- "YjZRiZJ3k7F2sJSARKkWI2S3e"
consumer_secret <- "zsxP8xTRdIniwsuOqtVrIlJxrvyylaqjBItdODzdXRmskFkwIN"
access_token <- "2363189640-eE7cgEI0oGWrocHnqTluZnf8Z8Fo8hQKFRL6obP"
access_secret <- "n6oFEKkWf5E3pA409OZvR5XinyzwkO0IJHAJa4opeV4fA"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# first saving positive and negative words to a text file
# i used the sentiments database from tidytext
# will see if i can just directly go into it in the future :)

dataframe <- data.frame(sentiments)
write.csv(dataframe, "sentiment.csv")



# sentiment analysis using twitter, or, identifying and catagorizing opinions 
# analyze comments to see if they are positive, negative or neutral
# positive and negative words are one database for us
# they then match against a sample statement then scored +1 0 and -1
# Positive Neutral or Negative

# constructive conjunction -- any sentene with "But" in it will be 
# evaluated sepereated. that way if you have a portion of the sentence that is 
# positive and the other negative, we can evaluate them seperately 
# this is called binary sentiment analysis

# demo: which football club has the most positive tweets 

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
pos.words = scan('C:/Users/dunwoodyweld123/Documents/R/project/TwitterSearch/positive.txt', what='character', comment.char=';')
neg.words = scan('C:/Users/dunwoodyweld123/Documents/R/project/TwitterSearch/negative.txt', what='character', comment.char=';')  

bscore = score.sentiments(tweet_df$text, pos.words, neg.words, .progress="text")
rscore = score.sentiments(tweet2_df$text, pos.words, neg.words, .progress="text")
hist(bscore$score)
hist(rscore$score)

tweet1 <- userTimeline("@barcalona", n=100)
tweet2 <- userTimeline("@realmadriden", n=100)
tweet_df <- tbl_df(map_df(tweet1, as.data.frame)) # turns into datafame
tweet2_df <- tbl_df(map_df(tweet2, as.data.frame)) # we only want the text out of it 
# so the dataframes go up to the b&r score and it only retrieves the actual text
  








