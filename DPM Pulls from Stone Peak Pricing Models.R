# Data Mining Stone Peak Pricing Models
# For DPM Pulls
# Nichole Freeman
# Start Date: 2.01.2018
# Last updated: 2.01.2018
# Completed:2.01.2018

# the purpose of this program is to grab data from a giant excel sheet
# that has circular references that takes to manually look up


######## Grab Rates For DPM #####################

require(readlx)
Final_Template1<- readxl::read_xlsx(file.choose(), col_names=TRUE, sheet = "Sheet1")

Final_Template<- Final_Template1[order(Final_Template1$`Project Name`),]

#Base outlines for what we want to produce
# First start with dates

#Set boundaries
Number_of_Iterations<- nrow(Final_Template)
i<- 1 

# Tells you whats on the list
# Pick the right sheet
# grabs info
# transfers to Final Template

for(y in 1:Number_of_Iterations) {
  print(Final_Template[i,c(1:3)])
  PVI<- readxl::read_xlsx(file.choose(), col_names=FALSE, sheet = "PVI Inputs")
  inverter_rate_cost_perW<- as.character(PVI[31,5])
  inverter_reserve_OpExp<- as.character(PVI[284,5])
  continue_inverter_in_reserve<- as.character(PVI[288,5])
  inverter_reserve_in_cash_flow_YN<- as.character(PVI[290,5])
  Final_Template[i,5] <- inverter_rate_cost_perW
  Final_Template[i,6] <- inverter_reserve_OpExp
  Final_Template[i,7] <- continue_inverter_in_reserve
  Final_Template[i,8] <- inverter_reserve_in_cash_flow_YN
  
  y<- y+1
  i<- i+1
  
}


write.table(Final_Template, file = "Data Mine SPP Models.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")








break
