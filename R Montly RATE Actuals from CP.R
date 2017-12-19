# Monthly RATE Data Template
# Nichole Freeman
# Start Date: 12.13.2017
# Last updated 12.13.2017
# Tentatively completed: 12.13.2017

# The purpose of this code is to collect an import from CenterPoint
# and recreate the data to go back into an excel sheet in a way 
# that prophix can read it automatically. These are for the Monthly RATE Values



MonData<- readxl::read_xlsx(file.choose(), col_names=TRUE)
# read in excel data from CenterPoint

# Estabolish new sites we want to see 
projects0 <- MonData[]
projects00 <- projects0[!(projects0$yyyy <= 2015),]
# takes only projects older than 2016
# takes away sites that do not have a facility name 
projects000<- projects00[!(projects00$facilityname == "NULL"),]
projects000<- projects00[!(projects00$yyyy == "NULL"),]
# takes away any null dates to have cleaner data

sites0<- data.frame((unique(projects000$facilityname)))

# these are all of the site names for reference 


#### trying this out ... get rid of lots of data to make it easier#######
clean<- data.frame(na.omit(projects000[5:9])) # this might just be a condition of the import
clean$date <- paste(clean$yyyy, clean$mm, sep = '')
clean<- with(clean,  clean[order(facilityname), ])
#transp<- data.frame(t(clean)) dont need transverse

######


#below are column names
x <- c("Project","2016M01","2016M02","2016M03",	"2016M04","2016M05",
       "2016M06","2016M07","2016M08",	"2016M09","2016M10","2016M11",
       "2016M12","2017M01","2017M02", "2017M03","2017M04", "2017M05",
       "2017M06","2017M07","2017M08", "2017M09","2017M10", "2017M11",
       "2017M12")
LCOL = length(x)+1
LROW = nrow(sites0)

PI <- data.frame(matrix(ncol = LCOL,nrow = LROW))
colnames(PI) = x
PI$Project<- sites0$X.unique.projects000.facilityname..
PI<- with(PI,  PI[order(Project), ])



YEARDATE<-  20161# set the year and date column we want to see first 
# n<- 1 unused counter 
# m<- 1 unused counter
# j<- 1 unused counter
i<- 1 #set  to 1 for testing / going forward
y<- 1 # set to 1
m<-1
DayPlay<- PI[] # I changed names fo when I tried different code
# then it wouldnt ruin dataframe 

for (y in 1:9){
  for (i in i:(nrow(DayPlay))){
    Day <- data.frame(subset(clean,clean$date == YEARDATE)) # narrow by date
    print1<- as.array(which(DayPlay$Project %in% Day$facilityname)) # pick like terms
    DayPlay[print1,y+1] = Day$rate
  }
  y<- y+1
  YEARDATE <- YEARDATE +1
}

yy<-1
iii<- 1
mm<-10
YEARDATE2<- 201610 


for (yy in 1:3){
  for (iii in 1:(nrow(DayPlay))){
    Day22 <- data.frame(subset(clean,clean$date == YEARDATE2)) # narrow by date
    print22<- as.array(which(DayPlay$Project %in% Day22$facilityname)) # pick like terms
    DayPlay[print22,mm+1] = Day22$rate
  }
  yy<- yy+1
  YEARDATE2 <- YEARDATE2 +1
  mm<- mm+1
}

####### 2017 Months


YEARDATE<-  20171# set the year and date column we want to see first 
# n<- 1 unused counter 
# m<- 1 unused counter
# j<- 1 unused counter
i<- 1 #set  to 1 for testing / going forward
y<- 1
m<-13# set to 1
# I changed names fo when I tried different code
# then it wouldnt ruin dataframe 

for (y in 1:9){
  for (i in 1:(nrow(DayPlay))){
    Day <- data.frame(subset(clean,clean$date == YEARDATE)) # narrow by date
    print1<- as.array(which(DayPlay$Project %in% Day$facilityname)) # pick like terms
    DayPlay[print1,m+1] = Day$rate
  }
  y<- y+1
  YEARDATE <- YEARDATE +1
  m<- m+1
}

yy<-1
yyy<-22
iii<- 1
YEARDATE2<- 201710 


for (yy in 1:2){
  for (iii in 1:(nrow(DayPlay))){
    Day22 <- data.frame(subset(clean,clean$date == YEARDATE2)) # narrow by date
    print22<- as.array(which(DayPlay$Project %in% Day22$facilityname)) # pick like terms
    DayPlay[print22,yyy+1] = Day22$rate
  }
  yyy<- yyy+1
  YEARDATE2 <- YEARDATE2 +1
}

write.table(DayPlay, file = "Montly Rate Values.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")


