# Data Mining Reconciliation Files
# For financial model, will pull and transform
# Nichole Phillips
# Start Date: 4.23.2018
# Completed Date: 
# Updated: 


require(readlx)
require(lubridate)
require(janitor)

Payment<- readxl::read_xlsx('C:/Users/nphillips/Documents/R/Code.Projects.Files/Prophix/Reconciliation Financials/2335_Template.xlsx', col_names=TRUE, sheet = "Payment")
Interest<- readxl::read_xlsx('C:/Users/nphillips/Documents/R/Code.Projects.Files/Prophix/Reconciliation Financials/2335_Template.xlsx', col_names=TRUE, sheet = "Interest")
Balance<- readxl::read_xlsx('C:/Users/nphillips/Documents/R/Code.Projects.Files/Prophix/Reconciliation Financials/2335_Template.xlsx', col_names=TRUE, sheet = "Balance")
                                
                                

count<- 1
End<- nrow(Payment)
a<- 1

for (a in 1:End){
  print(Payment[count,1])
  
  #Okay, process to convert dates first:
  
  Finance_Schedule <- readxl::read_xlsx(file.choose(), col_names=TRUE)
  
  GrabDate<- which(Finance_Schedule$X__1 %in% "Payment")
  dates<- Finance_Schedule[((GrabDate +1):800),1]
  dates<- data.frame(na.omit(dates))
  
  Num<- nrow(dates)
  i<-1
  rowstart<- GrabDate +1
  
  for(i in 1:Num){
    Convert <- janitor::excel_numeric_to_date(as.numeric(dates[i,1]), date_system = "modern")
    form<- format.Date(Convert, '%Y%m')
    y <- sub("\\s", "M", gsub('(.{4})', '\\1 ', form))
    Finance_Schedule[rowstart,1]<- y
    
    i<- i+1
    rowstart<- rowstart + 1
  }
  
  

######### Payment Column ###########
  
  
  GrabP<- which(Finance_Schedule$X__1 %in% "Payment")
  Pay0 <- data.frame(na.omit(Finance_Schedule[c((GrabP+1):(nrow(Finance_Schedule))),c(1,2)])) # Grab Column
  
  # Narrow down to 2016M12
  See<- Pay0[!grepl("2010", Pay0[,1]),]
  See<- See[!grepl("2011", See[,1]),]
  See<- See[!grepl("2012", See[,1]),]
  See<- See[!grepl("2013", See[,1]),]
  See<- See[!grepl("2014", See[,1]),]
  See<- See[!grepl("2015", See[,1]),]

  
  See<- See[!grepl("2016M01", See[,1]),]
  See<- See[!grepl("2016M02", See[,1]),]
  See<- See[!grepl("2016M03", See[,1]),]
  See<- See[!grepl("2016M04", See[,1]),]
  See<- See[!grepl("2016M05", See[,1]),]
  See<- See[!grepl("2016M06", See[,1]),]
  See<- See[!grepl("2016M07", See[,1]),]
  See<- See[!grepl("2016M08", See[,1]),]
  See<- See[!grepl("2016M09", See[,1]),]
  See<- See[!grepl("2016M10", See[,1]),]
  See<- See[!grepl("2016M11", See[,1]),]
  

  
  Pay0<- data.frame(na.omit(See))
  
  rownames(Pay0)<- Pay0[,1]
  Pay<- Pay0[2]
  
  # Populate sheet 
  cPay <- data.frame(which(colnames(Payment)%in% rownames(Pay)))
  
  wPay<-1
  for(wPay in 1:nrow(cPay)){
    value<- Pay[wPay,1]
    Payment[count,cPay[wPay,1]]<- value
    
    wPay<-wPay+1
  }
  
######### Payment Column ###########

######### Interest Column ###########
  
  GrabI<- which(Finance_Schedule$X__3 %in% "Interest")
  Intr0 <- data.frame(na.omit(Finance_Schedule[c((GrabI+1):(nrow(Finance_Schedule))),c(1,4)])) # Grab Column
  
  # Narrow down to 2016M12
  See<- Intr0[!grepl("2010", Intr0[,1]),]
  See<- See[!grepl("2011", See[,1]),]
  See<- See[!grepl("2012", See[,1]),]
  See<- See[!grepl("2013", See[,1]),]
  See<- See[!grepl("2014", See[,1]),]
  See<- See[!grepl("2015", See[,1]),]

  
  See<- See[!grepl("2016M01", See[,1]),]
  See<- See[!grepl("2016M02", See[,1]),]
  See<- See[!grepl("2016M03", See[,1]),]
  See<- See[!grepl("2016M04", See[,1]),]
  See<- See[!grepl("2016M05", See[,1]),]
  See<- See[!grepl("2016M06", See[,1]),]
  See<- See[!grepl("2016M07", See[,1]),]
  See<- See[!grepl("2016M08", See[,1]),]
  See<- See[!grepl("2016M09", See[,1]),]
  See<- See[!grepl("2016M10", See[,1]),]
  See<- See[!grepl("2016M11", See[,1]),]
  
  
  
  Intr0<- data.frame(na.omit(See))
  
  rownames(Intr0)<- Intr0[,1]
  Intr<- Intr0[2]
  
  # Populate sheet 
  cIntr <- data.frame(which(colnames(Interest)%in% rownames(Intr)))
  
  wIntr<-1
  for(wIntr in 1:nrow(cIntr)){
    value<- Intr[wIntr,1]
    Interest[count,cIntr[wIntr,1]]<- value
    
    wIntr<-wIntr+1
  }
  
  
  
  
  
  
  
######### Interest Column ###########

######### Balance Column ###########
  
  GrabB<- which(Finance_Schedule$X__4 %in% "Balance")
  Bal0 <- data.frame(na.omit(Finance_Schedule[c((GrabB+1):(nrow(Finance_Schedule))),c(1,5)])) # Grab Column
  
  # Narrow down to 2016M12
  See<- Bal0[!grepl("2010", Bal0[,1]),]
  See<- See[!grepl("2011", See[,1]),]
  See<- See[!grepl("2012", See[,1]),]
  See<- See[!grepl("2013", See[,1]),]
  See<- See[!grepl("2014", See[,1]),]
  See<- See[!grepl("2015", See[,1]),]

  
  See<- See[!grepl("2016M01", See[,1]),]
  See<- See[!grepl("2016M02", See[,1]),]
  See<- See[!grepl("2016M03", See[,1]),]
  See<- See[!grepl("2016M04", See[,1]),]
  See<- See[!grepl("2016M05", See[,1]),]
  See<- See[!grepl("2016M06", See[,1]),]
  See<- See[!grepl("2016M07", See[,1]),]
  See<- See[!grepl("2016M08", See[,1]),]
  See<- See[!grepl("2016M09", See[,1]),]
  See<- See[!grepl("2016M10", See[,1]),]
  See<- See[!grepl("2016M11", See[,1]),]
  
  
  
  Bal0<- data.frame(na.omit(See))
  
  rownames(Bal0)<- Bal0[,1]
  Bal<- Bal0[2]
  
  # Populate sheet 
  cBal <- data.frame(which(colnames(Balance)%in% rownames(Bal)))
  
  wBal<-1
  for(wBal in 1:nrow(cBal)){
    value<- Bal[wBal,1]
    Balance[count,cBal[wBal,1]]<- value
    
    wBal<-wBal+1
  }
  

######### Balance Column ###########
  
  # Very end of code
  
  a = a + 1
  count = count + 1
}



## Get rid of zeros

Payment[Payment == 0]<- NA
Interest[Interest == 0]<- NA
Balance[Balance == 0]<- NA

Payment[Payment == 1]<- NA
Interest[Interest == 1]<- NA
Balance[Balance == 1]<- NA


##

## Write csv

write.table(Payment, file = "Payment.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")

write.table(Interest, file = "Interest.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")

write.table(Balance, file = "Balance.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")


                               
                                