# Weather Data 
# Nichole Freeman
# Started: 12.19.2017

# Looking at different weather packages for data 

# install.packages("ROpenWeatherMap")
require(ROpenWeatherMap)
require(owmr)
library("httr")
library("RCurl")
library("jsonlite")

# Reference: https://github.com/mukul13/ROpenWeatherMap
# City ID's with long/lat 
#http://openweathermap.org/help/city_list.txt

# Need to have an API Key from Open Weather Map
# https://home.openweathermap.org
api_key<- "79fda871dee022e74c3d5f307e6ed642"

# you can grab data with using city name, key, or coordinates 
# can also use zipcode with country
# just use $ to grab variables
data<- ROpenWeatherMap::get_current_weather(api_key,city="portland")
str(data)
head(data)

# code from online to consider
# https://www.r-bloggers.com/turning-r-into-a-gis-mapping-the-weather-in-germany/

###################################################

# Trying a different package -- This one is actually really great
# this can do a 3 or 10 day forecast
# Reference:https://github.com/ALShum/rwunderground

#install.packages("rwunderground")
require(rwunderground)
set_api_key("d20e586981d06416")

#10 day forecast as a tibble 
forecast10day(set_location(territory = "Oregon", city = "Portland"))
# Historical forecast on a selected day
history(set_location(territory = "Oregon", city = "Portland"), 
                  date = 20171215)

#Returns locations of personal weather stations 
# along with URLs for their webcam image
webcam(set_location(territory = "Oregon", city = "Portland"))

#Tide high/low forecasts are available using tide 
# and hourly tide forecasts available using rawtide.
tide(set_location(territory = "Oregon", city = "Cannon Beach"))
rawtide(set_location(territory = "Oregon", city = "Cannon Beach"))


#Current conditions including current temperature, 
# weather condition, humidity, wind, feels-like, temperature, 
# barometric pressure, and visibility.
conditions(set_location(territory = "Oregon", city = "Portland"))

#Moon phase, sunrise and sunset times for today
astronomy(set_location(territory = "Oregon", city = "Portland"))



