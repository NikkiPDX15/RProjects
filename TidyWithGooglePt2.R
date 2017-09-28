# Using TidyText, Google Sheets and Twitter API pt2
# 9.28.2017
# Nichole Freeman

# Reference for set up Google Tweet Archive!
# https://www.youtube.com/watch?v=MGU7azCYFpw
# http://tidytextmining.com/tidytext.html
# really got the most out of this code, it's a good day!


library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(dplyr)
library(stringr)
library(tokenizers) # this package is pretty solid
library(tidyr)
library(reshape2)

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


sheet_df$TweetText

text <- as.character(sheet_df$TweetText)
# So now i have a long list of words 
Words <- as.character(tokenize_words(text, lowercase = TRUE, simplify = FALSE))

text_df <- data_frame(line=1:11081, text= Words)

 
# individual words now!
tidystuff<- text_df %>%
  unnest_tokens(word, text)

# if something sneaks through your code, you can make custom stop words
# and take them out of your data!
custom_stop_words <- bind_rows(data_frame(word = c("https", "rt", "t.co"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

# getting rid of stop words and what ever custom ones sneak in!
tidystuff <- tidystuff %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) 

# seeing the most used word into dataframe
highcount<- tidystuff %>%
  count(word, sort = TRUE) 

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

# taking a look at the sentiments database in tidytext
# three general purpose lexicons
# nrc -- yes/no category with more emotional discription
# bing -- positive or negative 
# AFINN -- runs on a n/p scale from -5 to 5 

# going to dive into tidy! 
# seeing what kind of values with words from tweets 

library(tidyr)

afinn <- tidystuff %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")


bing_and_nrc <- bind_rows(tidystuff %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidystuff %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = line %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# putting the different categories together for actuall models 
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# actual words that are pos/neg
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

# analyzing the most usesd words
# and putting the category next to them
# really what i was looking forward to doing!

bing_word_counts <- tidystuff %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts
#haha

#now data to be visually shown!
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# we can also make word clouds easily!!!

tidystuff %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
# comparison cloud! negative on top, positive on bottom
# super cool

tidystuff %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)



