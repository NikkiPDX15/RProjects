# Monthly Production Estimates Data Trasnform
# Nichole Freeman
# Start Date: 1.24.2018
# Last updated 1.24.2018


# The purpose of this code is to collect an import from CenterPoint
# and recreate the data to go back into an excel sheet in a way 
# that prophix can read it automatically. These are for the Monthly Estimated Values

#File Choose:
# YYYYMM x2 then the facilities ss from Centerpoint


# Lets start with upload template outlines
# Need to break it up into 2 sheets
require(readlx)

#Base outlines for what we want to produce
# First start with dates
First<- readxl::read_xlsx(file.choose(), col_names=TRUE, sheet = "2016-2036")
Second<- readxl::read_xlsx(file.choose(), col_names=TRUE, sheet = "2037-2047")

#Grab Facility Names for Projects/Numbers just for SSA1
Project<- readxl::read_xlsx(file.choose(), col_names = TRUE)
Project2<- Project[!(Project$yy <= "2015"),] # get rid of old dates
Project3<- Project2[Project2$corpid == "SSA1",] # only need SSA1 
Project4<- Project3[,c(5:9)]  # only info we need right now

# finally, grabbing project name
ProjNames<- data.frame((unique(Project4$facilityname)))

# First Sheet 2016 - 2036
First2<- First[]
First2<- data.frame(matrix(ncol = ncol(First)+1,
                                  nrow = nrow(ProjNames)))
colnames(First2)<- c("Project", colnames(First))

First2$Project <- ProjNames$X.unique.Project4.facilityname..

FirstSheet<- First2[]

# Second sheet
Second2<- Second[]
Second2<- data.frame(matrix(ncol = ncol(Second)+1,
                           nrow = nrow(ProjNames)))
colnames(Second2)<- c("Project", colnames(Second))

Second2$Project <- ProjNames$X.unique.Project4.facilityname..

SecondSheet<- Second2[]

########

# Okay
#FirstSheet == 2016-2036
#SecondSheet == 2037-2047
# Project4 == all the clean data we need
Project4$date <- paste(Project4$yy, Project4$mm, sep = '') #Make readable date
Project4<- with(Project4,  Project4[order(facilityname), ])# alphabetical

FirstSheet<- with(FirstSheet,  FirstSheet[order(Project), ])# alphabetical

SecondSheet<- with(SecondSheet,  SecondSheet[order(Project), ])# alphabetical

clean<- Project4[,c(1,4:6)] 
clean<- aggregate(usageamount~ date + facilityname, 
                  FUN = "sum", data = clean)

# Now clean is the data we use to grab!

##### FIRST SHEET########
#Pt.1
YEARDATE<- 20161 # set the year and date column 

i<- 1 
y<- 1 
d<-2
m<-1
FS1<- FirstSheet[] 

for (y in 1:20){
  for(m in 1:9){
    for (i in 1:(nrow(FirstSheet))){
      Mon <- data.frame(subset(clean,clean$date == YEARDATE))
      Mon<- with(Mon,  Mon[order(facilityname), ])# narrow by date
      print1<- as.array(which(FirstSheet$Project %in% Mon$facilityname)) # pick like terms
      FS1[print1,d] = Mon$usageamount
      
      }
    d<-d+1
    YEARDATE <- YEARDATE +1
    m<-m+1
  }
  d<- d+3
  YEARDATE<- YEARDATE +1
  m<-1
  y<- y+1
}

# Pt.2
YEARDATE2<- 201610 # set the year and date column 

ii<- 1 
yy<- 1 
dd<-11
mm<-1

for (yy in 1:20){
  for(mm in 1:3){
    for (ii in 1:(nrow(FirstSheet))){
      Mon2 <- data.frame(subset(clean,clean$date == YEARDATE2))
      Mon2<- with(Mon2,  Mon2[order(facilityname), ])# narrow by date
      print12<- as.array(which(FirstSheet$Project %in% Mon2$facilityname)) # pick like terms
      FS1[print12,dd] = Mon2$usageamount
      
    }
    dd<-dd+1
    YEARDATE2 <- YEARDATE2 +1
    mm<-mm+1
  }
  dd<- dd+9
  YEARDATE2<- YEARDATE2 +97
  mm<-1
  yy<- yy+1
}


write.table(FS1, file = "Montly Estimated Production Values.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")


#####################################

