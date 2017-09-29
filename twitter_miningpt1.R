# Twitter Data Mining Project Exploration part1 
# 9.27.2017
# Nichole Freeman

# Exploring mor datamining options 
# kept the same code from twitter_search for initial access
# info on tweets 
# Twitter trends 


install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud") 

require(twitteR)
require(RCurl)
require(tm)  # most popular text mining 
require(wordcloud)  # creates text visuals 

consumer_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXX"
consumer_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# gives us authorization to get into twitter

# getting tweets ... $appl gives us tweets that have to do with money 
# and apple products
#  turning the list of tweets into a data frame

tweets <- searchTwitter("$appl", n=10, lang = "en")
tweetsdf <- twListToDF(tweets)
# data frame gives you info on the tweet!
# was it favorited, replied to, screen name, date created & etc. 


# How to look at trends by locations
trends = availableTrendLocations()
# this will give you a list of places to gather trends from certain locations
# number 1 is world wide, so lets look 
worldTrend <- getTrends(23424977)  # this is the number for the united states

# User timeline info
# two newest tweets
 me <- getUser('nikkipdx15')  # my twitter page 
userTimeline(me, n=2)     # fetches two of the last tweets




