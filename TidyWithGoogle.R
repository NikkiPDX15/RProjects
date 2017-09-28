# Using TidyText, Google Sheets and Twitter API
# 9.28.2017
# Nichole Freeman

# Reference for set up Google Tweet Archive!
# https://www.youtube.com/watch?v=MGU7azCYFpw
# http://tidytextmining.com/tidytext.html


# reading file from web, the google archive app
# super useful app! really recommend it 

install.packages("tokenizers")


library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(dplyr)
library(stringr)
library(tokenizers) # this package is pretty solid

# going to read the CSV file that we published 

MyURL <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQ959Ssp0-ESR439IcBp30fp6b8Bw2j8ZQoGumPJM4ni349sX3QIaUMmpXrJ4Esj4BxoW3aBDlsWj0s/pub?gid=1279454736&single=true&output=csv'
sheet_df <- read.csv(url(MyURL))
head(sheet_df)

#Lets give the columns names 

colnames(sheet_df) <- c("Date",	"ScreenName", "FullName",
                        "TweetText", "TweetID","App","Followers",
                        "Follows",	"Retweets", "Favorites",
                        "Verfied", "UserSince","Location","Bio",
                        "ProfileImage",	"GoogleMaps")

# Notes from before on Tidytext
# We need to convert this so that it has one-token-per-document-per-row.
# A token is a meaningful unit of text, most often a word, that we are interested 
# in using for further analysis, and tokenization is the process of splitting 
# text into tokens.

# i actually found a tokenizer package yay!

# taking a look at the sentiments database in tidytext
# three general purpose lexicons
# nrc -- yes/no category with more emotional discription
# bing -- positive or negative 
# AFINN -- runs on a n/p scale from -5 to 5 

sheet_df$TweetText

text <- as.character(sheet_df$TweetText)
# So now i have a long list of words 
Words <- as.character(tokenize_words(text, lowercase = TRUE, simplify = FALSE))

text_df <- data_frame(line=1:11081, text= Words)

 
# individual words now!
tidystuff<- text_df %>%
  unnest_tokens(word, text)

# getting rid of stop words
tidystuff <- tidystuff %>%
  anti_join(stop_words)

# seeing the most used word into dataframe
highcount<- tidystuff %>%
  count(word, sort = TRUE) 

#see <- apply(highcount, 2, function(x) which(x %in% c("https", "t.co", "rt")))
#if we wanted to get rid of rows ... didnt feel like working on it 

# plotting visualization to show how many times the word was used 

library(ggplot2)

tidystuff %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


