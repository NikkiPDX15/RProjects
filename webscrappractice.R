# Turned into a webscrape practice

# Started: 11.07.2017
# Last updated: 11.07.2017
# Nichole Freeman


# scape, crawl and get URL data
# Just looking at alternatives 


##########################################################

require(twitteR) # Used to connect our R to Twitter 
require(RCurl) # Retrieves content from wed server 
require(tm)  # most popular text mining 
library(rvest) # used a lot in data scraping 
library(stringr)
library(dplyr)
library(rio) # can import almost any file type


# I created a seperate file that hosts my twitter autorization 
# that way my twitter app secrets are kept off GitHub :)

# Call Authorization file
source('C:/Users/dunwoodyweld123/Documents/R/project/TwitterAuth.R')


#####
#Website Scraping first

require(RCurl) # get content from URL

web.request <- getURL("http://www.bestcollegesonline.com/blog/50-essential-twitter-feeds-for-stem-educators/",
                      ssl.verifypeer = FALSE)
# What is returned is a all the info from page with a vector length of 1
print(web.request)

#now we want just the main content
# need to creat a tree structure using XML package
require(XML)

web.request <- htmlTreeParse(web.request, useInternal = TRUE)
print(web.request)


####
# Going to try to scrape with rvest

require(rvest)

url <- 'http://www.bestcollegesonline.com/blog/50-essential-twitter-feeds-for-stem-educators/'
webpage <- read_html(url)
### #####
# try again?
#### AHHHHH

####
### okay new concept
# web crawling info: https://github.com/salimk/Rcrawler


require(Rcrawler)
Rcrawler(Website = "http://www.bestcollegesonline.com/blog/50-essential-twitter-feeds-for-stem-educators/",
         no_cores = 1, no_conn = 1)

# no_cores specify how many processes will execute the task
# no_conn specify how many HTTP requests will be sent simultaneously (in parallel).

# comes up with a dataframe called INDEX
# representing the general URL index, 
# which includes all crawled/scraped web pages with 
# their details (content type, HTTP state, the number of out-links 
# and in-links, encoding type, and level). 

ListProjects()
# lists crawling project folders
# then pickup project and turn it into a vector

MyDATA<-LoadHTMLFiles("bestcollegesonline.com-071556", type = "vector")


#Or just look for key words... this might take a while
Rcrawler(Website = "http://www.bestcollegesonline.com/blog/50-essential-twitter-feeds-for-stem-educators/",KeywordsFilter = c("twitter"))

AGAIN <- LoadHTMLFiles("bestcollegesonline.com-071611", type = "vector")

# Or just limit it to one level deep

Rcrawler(Website= "http://www.bestcollegesonline.com/blog/50-essential-twitter-feeds-for-stem-educators/",
         MaxDepth=1, urlregexfilter = "blog/50-essential-twitter-feeds-for-stem-educators/")



