# Data Mining Stone Peak Pricing Models
# For DPM Pulls word recognition
# Nichole Freeman
# Start Date: 3.29.2018
# Completed Date: 3.29.2018
# Updated: 3.29.2018


# the purpose of this program is to grab data from a giant excel sheet
# that has circular references that takes to manually look up

# this version uses word recognition


######## Grab Rates For DPM #####################

require(readlx)
require(lubridate)
require(janitor)


Final_Template1<- readxl::read_xlsx(file.choose(), col_names=TRUE, sheet = "Sheet1")

Final_Template<- Final_Template1[order(Final_Template1$`Project Name`),]


#Base outlines for what we want to produce
# First start with dates

#Set boundaries
Number_of_Iterations<- nrow(Final_Template)
i<- 1 

for(y in 1:Number_of_Iterations) {
  print(Final_Template[i,c(1:3)])
  PVI<- readxl::read_xlsx(file.choose(), col_names=FALSE, sheet = "PVI Inputs")
  MonthlyFS<- readxl::read_xlsx(file.choose(), col_names=FALSE, sheet = "Monthly FS")
  
  inverter_rate_cost_perW0<- which(PVI$X__4 %in% "Inverters")
  inverter_rate_cost_perW<- as.character(PVI[inverter_rate_cost_perW0,5])
  
  inverter_reserve_OpExp0<- which(PVI$X__4 %in% "Inverter Reserve")
  inverter_reserve_OpExp<- as.character(PVI[inverter_reserve_OpExp0,5])
  
  
  continue_inverter_in_reserve0<- which(PVI$X__4 %in% "Continue Inverter Reserve?")
  continue_inverter_in_reserve<- as.character(PVI[continue_inverter_in_reserve0,5])
  
  inverter_reserve_in_cash_flow_YN0<- which(PVI$X__4 %in% "Inverter Reserve In Cash Flow?")
  inverter_reserve_in_cash_flow_YN<- as.character(PVI[inverter_reserve_in_cash_flow_YN0,5])
  
  PPA_RATE0<- which(PVI$X__4 %in% "Sales Price/kWh")
  PPA_RATE<- as.character(PVI[PPA_RATE0,5])
  
  NOEBOLEASEBO0<- which(PVI$X__4 %in% "Buyout Amount")
    #NoEBO Lease option
  NOEBOLEASEBO<- as.character(PVI[NOEBOLEASEBO0,7])
  #EBO 
  YESEBOLEASEBO<- as.character(PVI[NOEBOLEASEBO0,5])
  
  ## Lease term (yrs)
  LeaseTerms0<- which(PVI$X__4 %in% "Lease Term (years)")
  LeaseTerms<- as.character(PVI[LeaseTerms0,5])
  
  Early_BuyOut_Term0<- which(PVI$X__4 %in% "Early Buyout Term (years)")
  Early_BuyOut_Term<- as.character(PVI[Early_BuyOut_Term0,5])
  
  Sale_Leaseback_Proceeds0<- which(PVI$X__10 %in% "Sale-Leaseback Proceeds")
  Sale_Leaseback_Proceeds<- as.character(PVI[Sale_Leaseback_Proceeds0,11])
  
  
  LeaseBuyOut0<- MonthlyFS[c(10:nrow(MonthlyFS)),c(1,38)]
  LeaseBuyOut00<- as.array(which(LeaseBuyOut0[,2] != 0))
  LeaseBuyOut1<- LeaseBuyOut0[LeaseBuyOut00,]
  
  LeaseBuyOut<- janitor::excel_numeric_to_date(as.numeric(LeaseBuyOut1[1,1]), date_system = "modern")
  LeaseBuyOut<- format.Date(LeaseBuyOut, '%m/%d/%Y')
  
  CapexTotal0<- which(PVI$X__4 %in% "Total Cost")
  CapexTotal<- as.character(PVI[CapexTotal0, 7])
  
  
  # Template Layout
  Final_Template[i,5] <- inverter_rate_cost_perW
  Final_Template[i,6] <- inverter_reserve_OpExp
  Final_Template[i,7] <- continue_inverter_in_reserve
  Final_Template[i,8] <- inverter_reserve_in_cash_flow_YN
  Final_Template[i,9] <- PPA_RATE
  Final_Template[i,10]<-  YESEBOLEASEBO
  Final_Template[i,11]<- NOEBOLEASEBO
  Final_Template[i,12]<- LeaseTerms
  Final_Template[i,13]<- Early_BuyOut_Term
  Final_Template[i,14]<- Sale_Leaseback_Proceeds
  Final_Template[i,15]<- LeaseBuyOut
  Final_Template[i,17]<- CapexTotal

  
  
  y<- y+1
  i<- i+1
  
}


#For Non EBO selection

Number_of_Iterations0<- nrow(Final_Template)
ii<- 1 

for(yy in 1:Number_of_Iterations0) {
  print(Final_Template[ii,c(1:3)])
  MonthlyFS<- readxl::read_xlsx(file.choose(), col_names=FALSE, sheet = "Monthly FS")

  LeaseBuyOut0<- MonthlyFS[c(10:nrow(MonthlyFS)),c(1,38)]
  LeaseBuyOut00<- as.array(which(LeaseBuyOut0[,2] != 0))
  LeaseBuyOut1<- LeaseBuyOut0[LeaseBuyOut00,]
  
  LeaseBuyOutN<- janitor::excel_numeric_to_date(as.numeric(LeaseBuyOut1[1,1]), date_system = "modern")
  LeaseBuyOutN<- format.Date(LeaseBuyOutN, '%m/%d/%Y')
  
  Final_Template[ii,16]<- LeaseBuyOutN
  
  
  yy<- yy+1
  ii<- ii+1
  
}



write.table(Final_Template, file = "Data Mine SPP Models.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")

  

  

