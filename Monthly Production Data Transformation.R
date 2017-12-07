# Monthly Production Data Template
# Nichole Freeman
# Start Date: 12.5.2017
# Last updated 12.6.2017
# Tentatively completed: 12.6.2017

# The purpose of this code is to collect an import from CenterPoint
# and recreate the data to go back into an excel sheet in a way 
# that prophix can read it automatically. These are for the Monthly Values



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
clean<- data.frame(projects000[5:8]) # this might just be a condition of the import
clean$date <- paste(clean$yyyy, clean$mm, sep = '')
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


#################### 2016####################
#### 2016M1
S161 <- data.frame(subset(clean,clean$date == 20161)) # narrow by date
print1<- as.array(which(PI$Project %in% S161$facilityname)) # pick like terms
PI[print1,2] = S161$usageamount # put into data frame
#### 2016M2
S162 <- data.frame(subset(clean,clean$date == 20162))
print2<- as.array(which(PI$Project %in% S162$facilityname))
PI[print2,3] = S162$usageamount
#### 2016M3
S163 <- data.frame(subset(clean,clean$date == 20163))
print3<- as.array(which(PI$Project %in% S163$facilityname))
PI[print3,4] = S163$usageamount
#### 2016M4
S164 <- data.frame(subset(clean,clean$date == 20164))
print4<- as.array(which(PI$Project %in% S164$facilityname))
PI[print4,5] = S164$usageamount
#### 2016M5
S165 <- data.frame(subset(clean,clean$date == 20165))
print5<- as.array(which(PI$Project %in% S165$facilityname))
PI[print5,6] = S165$usageamount
#### 2016M6
S166 <- data.frame(subset(clean,clean$date == 20166))
print6<- as.array(which(PI$Project %in% S166$facilityname))
PI[print6,7] = S166$usageamount
#### 2016M7
S167 <- data.frame(subset(clean,clean$date == 20167))
print7<- as.array(which(PI$Project %in% S167$facilityname))
PI[print7,8] = S167$usageamount
#### 2016M8
S168 <- data.frame(subset(clean,clean$date == 20168))
print8<- as.array(which(PI$Project %in% S168$facilityname))
PI[print8,9] = S168$usageamount
#### 2016M9
S169 <- data.frame(subset(clean,clean$date == 20169))
print9<- as.array(which(PI$Project %in% S169$facilityname))
PI[print9,10] = S169$usageamount
#### 2016M10
S1610 <- data.frame(subset(clean,clean$date == 201610))
print10<- as.array(which(PI$Project %in% S1610$facilityname))
PI[print10,11] = S1610$usageamount
#### 2016M11
S1611 <- data.frame(subset(clean,clean$date == 201611))
print11<- as.array(which(PI$Project %in% S1611$facilityname))
PI[print11,12] = S1611$usageamount
#### 2016M12
S1612 <- data.frame(subset(clean,clean$date == 201612))
print12<- as.array(which(PI$Project %in% S1612$facilityname))
PI[print12,13] = S1612$usageamount
#########################################################


#################### 2017####################
#### 2017M1
S171 <- data.frame(subset(clean,clean$date == 20171)) # narrow by date
print1<- as.array(which(PI$Project %in% S171$facilityname)) # pick like terms
PI[print1,14] = S171$usageamount # put into data frame
#### 2017M2
S172 <- data.frame(subset(clean,clean$date == 20172))
print2<- as.array(which(PI$Project %in% S172$facilityname))
PI[print2,15] = S172$usageamount
#### 2017M3
S173 <- data.frame(subset(clean,clean$date == 20173))
print3<- as.array(which(PI$Project %in% S173$facilityname))
PI[print3,16] = S173$usageamount
#### 2017M4
S174 <- data.frame(subset(clean,clean$date == 20174))
print4<- as.array(which(PI$Project %in% S174$facilityname))
PI[print4,17] = S174$usageamount
#### 2017M5
S175 <- data.frame(subset(clean,clean$date == 20175))
print5<- as.array(which(PI$Project %in% S175$facilityname))
PI[print5,18] = S175$usageamount
#### 2017M6
S176 <- data.frame(subset(clean,clean$date == 20176))
print6<- as.array(which(PI$Project %in% S176$facilityname))
PI[print6,19] = S176$usageamount
#### 2017M7
S177 <- data.frame(subset(clean,clean$date == 20177))
print7<- as.array(which(PI$Project %in% S177$facilityname))
PI[print7,20] = S177$usageamount
#### 2017M8
S178 <- data.frame(subset(clean,clean$date == 20178))
print8<- as.array(which(PI$Project %in% S178$facilityname))
PI[print8,21] = S178$usageamount
#### 2017M9
S179 <- data.frame(subset(clean,clean$date == 20179))
print9<- as.array(which(PI$Project %in% S179$facilityname))
PI[print9,22] = S179$usageamount
#### 2017M10
S1710 <- data.frame(subset(clean,clean$date == 201710))
print10<- as.array(which(PI$Project %in% S1710$facilityname))
PI[print10,23] = S1710$usageamount
#### 2017M11
S1711 <- data.frame(subset(clean,clean$date == 201711))
print11<- as.array(which(PI$Project %in% S1711$facilityname))
PI[print11,24] = S1711$usageamount
#### 2017M12
S1712 <- data.frame(subset(clean,clean$date == 201712))
print12<- as.array(which(PI$Project %in% S1712$facilityname))
PI[print12,25] = S1712$usageamount
#########################################################


write.table(PI, file = "Montly Production Values.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")






