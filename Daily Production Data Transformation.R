# Daily Production Data Transformation
# Nichole Freeman
# Start Date: 12.6.2017
# Last updated 12.7.2017
# Tentatively completed: 12.7.2017

# The purpose of this code is to collect an import from CenterPoint
# and recreate the data to go back into an excel sheet in a way 
# that prophix can read it automatically.This is for the Daily Values 

DailyData<- readxl::read_xlsx(file.choose(), col_names=TRUE)

# to build the new template, we need headers and I dont feel like doing 
# all of that work by hand 
# Let's get all of those headers for template by importing from excel
# Dont forget that 2016 and 2020 had an extra day :)

HeaderData<- readxl::read_xlsx(file.choose(), col_names = FALSE)
# will change above to solid location later
# Separate headers by years -- lots of data, all different sheets
Header2016<- HeaderData[1,]
Header2017<- HeaderData[2,]
Header2018<- HeaderData[3,]
Header2019<- HeaderData[4,]
Header2020<- HeaderData[5,]


#################Starting with the 2016 Files########################
DateCut<- DailyData[] # create data frame to edit data
DateCut<- DateCut[!(DateCut$effdatech <= "2015-12-31"),] # only from 2016 on

DateCut$effdatech<- format(as.Date(DateCut$effdatech), format='%Y%j')
# Above just turned the yyyy/mm/dd to yyyy + julian day


clean0<- data.frame(na.omit(DateCut[5:7]))
clean<- aggregate(usageamount ~ effdatech + facilityname, FUN = "sum", data = clean0)
# Merged and sumed all of the data with same date
# turns out some sites have mulitple production rates per day


# Only grabbing facility name, date and production value
ProjNames0<- data.frame((unique(clean$facilityname)))
ProjNames<- na.omit(ProjNames0) # easy way I found to omit NULL data!
# We will use this a poject names criteria 

# DailyData+YYYY will be the template for the import to prophix
# Use headers from above 
DailyData2016<- data.frame(matrix(ncol = ncol(Header2016)+1,
                                  nrow = nrow(ProjNames)))
# Now lets add the column names
colnames(DailyData2016)<-c("Project", Header2016[1,])
# Now add Sites
DailyData2016$Project <- ProjNames[,1]

# data converted into new DailyData For 2016
# Start Year Date and iterattion for column
      # since we start the first column with projects, we need n+1
# Need to see why it wont work :(

YEARDATE<- 2016001 # set the year and date column we want to see first 
# n<- 1 unused counter 
# m<- 1 unused counter
# j<- 1 unused counter
i<- 1 #set  to 1 for testing / going forward
y<- 1 # set to 1
DayPlay<- DailyData2016[] # I changed names fo when I tried different code
                          # then it wouldnt ruin dataframe 
for (y in i:ncol(DayPlay)){
  for (i in 1:(nrow(DayPlay)-1)){
     Day <- data.frame(subset(clean,clean$effdatech == YEARDATE))
     Day2<- data.frame(subset(DayPlay,(DayPlay$Project %in% Day$facilityname)))
      Result<- ifelse(!is.na(DayPlay$Project %in% Day$facilityname), Day$usageamount,0)
        DayPlay[,y+1] = Result
  }
  y<- y+1
  YEARDATE <- YEARDATE +1
}

#Above line of thinking:
# go through each column first by picking year/date that matches w/in dataframes
# then to the next date, and repeat

# then write to file
write.table(DayPlay, file = "Daily Production Values 2016.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")




###################### 2017 Files##################################

DateCut2<- DailyData[] # create data frame to edit data
DateCut2<- DateCut2[!(DateCut2$effdatech <= "2016-12-31"),] # only from 2017 on

DateCut2$effdatech<- format(as.Date(DateCut2$effdatech), format='%Y%j')

clean02<- data.frame(na.omit(DateCut2[5:7]))
clean2<- aggregate(usageamount ~ effdatech + facilityname, FUN = "sum", data = clean02)

# Only grabbing facility name, date and production value
ProjNames02<- data.frame((unique(clean2$facilityname)))
ProjNames2<- na.omit(ProjNames02) 
# We will use this a poject names criteria 

# DailyData+YYYY will be the template for the import to prophix
# Use headers from above 
DailyData2017<- data.frame(matrix(ncol = ncol(Header2017)+1,
                                  nrow = nrow(ProjNames2)))
# Now lets add the column names
colnames(DailyData2017)<-c("Project", Header2017[1,])
# Now add Sites
DailyData2017$Project <- ProjNames2[,1]

# data converted into new DailyData For 2016
# Start Year Date and iterattion for column
# since we start the first column with projects, we need n+1
# Need to see why it wont work :(

YEARDATE2<- 2017001 # set the year and date column 

ii<- 1 
yy<- 1 
DayPlay2<- DailyData2017[] 

for (yy in yy:ncol(DayPlay2)){
  for (ii in 1:(nrow(DayPlay2))){
    Day02 <- data.frame(subset(clean2,clean2$effdatech == YEARDATE2))
    Day22<- data.frame(subset(DayPlay2,(DayPlay2$Project %in% Day02$facilityname)))
    Result2<- ifelse(!is.na(DayPlay2$Project %in% Day02$facilityname), Day02$usageamount,0)
    DayPlay2[,yy+1] = Result2
  }
  yy<- yy+1
  YEARDATE2 <- YEARDATE2 +1
}


# then write to file
write.table(DayPlay2, file = "Daily Production Values 2017.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")






