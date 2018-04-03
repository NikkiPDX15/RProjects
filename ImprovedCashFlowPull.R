# IMPROVED Cash Flow Recuglar Cube upload 
# Data Mining Stone Peak Pricing Models
# Nichole Freeman
# Start Date: 3.29.2018
# Completed Date: 
# Last updated: 3.30.2018

require(readlx)
require(lubridate)
require(janitor)




#selects file you you --- DO NOT MOVE TEMPLATE LOCATION


Sheet1_Temp<- readxl::read_xlsx('C:/Users/nphillips/Documents/R/Code.Projects.Files/Prophix/Stone Peak Pricing models/CashFlowsTemplate.xlsx', col_names=TRUE, sheet = "1216 to 0237")
Sheet2_Temp<- readxl::read_xlsx('C:/Users/nphillips/Documents/R/Code.Projects.Files/Prophix/Stone Peak Pricing models/CashFlowsTemplate.xlsx', col_names=TRUE, sheet = "0337 to 1250")

# Variables with templates attached

Grant_Rebates1<- Sheet1_Temp
Grant_Rebates2<- Sheet2_Temp

Lease_Payments1<- Sheet1_Temp
Lease_Payments2<- Sheet2_Temp

SLB_Proceeds1<- Sheet1_Temp
SLB_Proceeds2<- Sheet2_Temp

SREC_Rev1<- Sheet1_Temp
SREC_Rev2<- Sheet2_Temp

Property_Tax1<- Sheet1_Temp
Property_Tax2<- Sheet2_Temp

Site_Lease1<- Sheet1_Temp
Site_Lease2<- Sheet2_Temp




Count<- 1
End<- nrow(Sheet1_Temp)
a<- 1

for (a in 1:End){
    print(Sheet1_Temp[Count,5])
    
    #Okay, process to convert dates first:
    
    MonthlyFS<- readxl::read_xlsx(file.choose(), col_names=TRUE, sheet = "Monthly FS")

      
    dates<- MonthlyFS[(9:800),1]
    dates<- data.frame(na.omit(dates))
        
    Num<- nrow(dates)
    i<-1
    rowstart<- 9
        
      for(i in 1:Num){
        Convert <- janitor::excel_numeric_to_date(as.numeric(dates[i,1]), date_system = "modern")
        form<- format.Date(Convert, '%Y%m')
        y <- sub("\\s", "M", gsub('(.{4})', '\\1 ', form))
        MonthlyFS[rowstart,1]<- y
          
        i<- i+1
        rowstart<- rowstart + 1
      }

    
      
    
    
############### Grant Rebates ######################  
    
    GR0 <- data.frame(na.omit(MonthlyFS[c(10:(nrow(MonthlyFS))),c(1,33)])) # Grab Column
    
    # Narrow down to 2016 if neeeded
    See<- GR0[!grepl("2010", GR0[,1]),]
    See<- See[!grepl("2011", See[,1]),]
    See<- See[!grepl("2012", See[,1]),]
    See<- See[!grepl("2013", See[,1]),]
    See<- See[!grepl("2014", See[,1]),]
    See<- See[!grepl("2015", See[,1]),]
    
    GR0<- data.frame(na.omit(See))
    
    rownames(GR0)<- GR0[,1]
    GR<- GR0[2]

    
    # First Sheet  
    cGR <- data.frame(which(colnames(Grant_Rebates1)%in% rownames(GR)))
    
    wGR<-1
    for(wGR in 1:nrow(cGR)){
      value<- GR[wGR,1]
      Grant_Rebates1[Count,cGR[wGR,1]]<- value
      
      wGR<-wGR+1
    }
    
    # Second Sheet
    dGR<- data.frame(which(colnames(Grant_Rebates2) %in% rownames(GR)))
    
    wwGR <- nrow(GR) - nrow(cGR)
    STARTGR<- 1
    
    for(STARTGR in 1:nrow(dGR)){
      value<- GR[wwGR,1]
      Grant_Rebates2[Count,dGR[STARTGR,1]]<- value
      
      wwGR<-wwGR+1
      STARTGR<- STARTGR + 1
    }

############### GR ABOVE ###########################
    
############### LEASE PAYMENTS ######################  
    LP0 <- data.frame(na.omit(MonthlyFS[c(10:(nrow(MonthlyFS))),c(1,51)]))  #  Column
    
    
    # Narrow down to 2016 if neeeded
    See<- LP0[!grepl("2010", LP0[,1]),]
    See<- See[!grepl("2011", See[,1]),]
    See<- See[!grepl("2012", See[,1]),]
    See<- See[!grepl("2013", See[,1]),]
    See<- See[!grepl("2014", See[,1]),]
    See<- See[!grepl("2015", See[,1]),]
    
    LP0<- data.frame(na.omit(See))
    
    rownames(LP0)<- LP0[,1]
    LP<- LP0[2]
    
    
    # First Sheet  
    cLP <- data.frame(which(colnames(Lease_Payments1)%in% rownames(LP)))
    
    wLP<-1
    for(wLP in 1:nrow(cLP)){
      value<- LP[wLP,1]
      Lease_Payments1[Count,cLP[wLP,1]]<- value
      
      wLP<-wLP+1
    }
    
    # Second Sheet
    dLP<- data.frame(which(colnames(Lease_Payments2) %in% rownames(LP)))
    
    wwLP <- nrow(LP) - nrow(cLP)
    STARTLP<- 1
    
    for(STARTLP in 1:nrow(dLP)){
      value<- LP[wwLP,1]
      Lease_Payments2[Count,dLP[STARTLP,1]]<- value
      
      wwLP<-wwLP+1
      STARTLP<- STARTLP + 1
    }
    
    
############### LP ABOVE ###########################
    
    
############### SLB Proceeds first######################
    SLB0 <- data.frame(na.omit(MonthlyFS[c(10:(nrow(MonthlyFS))),c(1,35)]))  # Grab Column
    
    # Narrow down to 2016 if neeeded
    See<- SLB0[!grepl("2010", SLB0[,1]),]
    See<- See[!grepl("2011", See[,1]),]
    See<- See[!grepl("2012", See[,1]),]
    See<- See[!grepl("2013", See[,1]),]
    See<- See[!grepl("2014", See[,1]),]
    See<- See[!grepl("2015", See[,1]),]
    
    SLB0<- data.frame(na.omit(See))
    
    
    rownames(SLB0)<- SLB0[,1]
    SLB<- SLB0[2]
  
    
    # First Sheet  
    c <- data.frame(which(colnames(SLB_Proceeds1)%in% rownames(SLB)))
    
    w<-1
    for(w in 1:nrow(c)){
      value<- SLB[w,1]
      SLB_Proceeds1[Count,c[w,1]]<- value
      
      w<-w+1
    }
    
    # Second Sheet
    d<- data.frame(which(colnames(SLB_Proceeds2) %in% rownames(SLB)))
    
    ww <- nrow(SLB) - nrow(c)
    START<- 1
    
    for(START in 1:nrow(d)){
      value<- SLB[ww,1]
      SLB_Proceeds2[Count,d[START,1]]<- value
      
      ww<-ww+1
      START<- START + 1
    }
    
###############################SLB PROCEEDS ABOVE #################################  
    
    
############### SREC REV below ######################  
    SRECREV0 <- data.frame(na.omit(MonthlyFS[c(10:(nrow(MonthlyFS))),c(1,41)]))  #  Column
    
    # Narrow down to 2016 if neeeded
    See<- SRECREV0[!grepl("2010", SRECREV0[,1]),]
    See<- See[!grepl("2011", See[,1]),]
    See<- See[!grepl("2012", See[,1]),]
    See<- See[!grepl("2013", See[,1]),]
    See<- See[!grepl("2014", See[,1]),]
    See<- See[!grepl("2015", See[,1]),]
    
    SRECREV0<- data.frame(na.omit(See))
    
    rownames(SRECREV0)<- SRECREV0[,1]
    SRECREV<- SRECREV0[2]
    
    
    # First Sheet  
    cSRECREV <- data.frame(which(colnames(SREC_Rev1)%in% rownames(SRECREV)))
    
    wSRECREV<-1
    for(wSRECREV in 1:nrow(cSRECREV)){
      value<- SRECREV[wSRECREV,1]
      SREC_Rev1[Count,cSRECREV[wSRECREV,1]]<- value
      
      wSRECREV<-wSRECREV+1
    }
    
    # Second Sheet
    dSRECREV<- data.frame(which(colnames(SREC_Rev2) %in% rownames(SRECREV)))
    
    wwSRECREV <- nrow(SRECREV) - nrow(cSRECREV)
    STARTSRECREV<- 1
    
    for(STARTSRECREV in 1:nrow(dSRECREV)){
      value<- SRECREV[wwSRECREV,1]
      SREC_Rev2[Count,dSRECREV[STARTSRECREV,1]]<- value
      
      wwSRECREV<-wwSRECREV+1
      STARTSRECREV<- STARTSRECREV + 1
    }
    
############### SREC REV ABOVE ######################
    
    
############### Prop Tax ############################  
    
    # Okay, so Property tax also has two columns based off a condition
    # so here we go makin it!!
    
    if("PPT" %in% MonthlyFS[8,49] || "PILOT" %in% MonthlyFS[8,49]){
      ### Now get the PPT column
      PPT0 <- data.frame(na.omit(MonthlyFS[c(10:(nrow(MonthlyFS))),c(1,49)]))
      
      # Narrow down to 2016 if neeeded
      See<- PPT0[!grepl("2010", PPT0[,1]),]
      See<- See[!grepl("2011", See[,1]),]
      See<- See[!grepl("2012", See[,1]),]
      See<- See[!grepl("2013", See[,1]),]
      See<- See[!grepl("2014", See[,1]),]
      See<- See[!grepl("2015", See[,1]),]
      
      PPT0<- data.frame(na.omit(See))
      
      rownames(PPT0)<- PPT0[,1]
      PPT<- PPT0[2]
      
      
      # First Sheet  
      cPPT <- data.frame(which(colnames(Property_Tax1)%in% rownames(PPT)))
      
      wPPT<-1
      for(wPPT in 1:nrow(cPPT)){
        value<- PPT[wPPT,1]
        Property_Tax1[Count,cPPT[wPPT,1]]<- value
        
        wPPT<-wPPT+1
      }
      
      # Second Sheet
      dPPT<- data.frame(which(colnames(Property_Tax2) %in% rownames(PPT)))
      
      wwPPT <- nrow(PPT) - nrow(cPPT)
      STARTPPT<- 1
      
      for(STARTPPT in 1:nrow(dPPT)){
        value<- PPT[wwPPT,1]
        Property_Tax2[Count,dPPT[STARTPPT,1]]<- value
        
        wwPPT<-wwPPT+1
        STARTPPT<- STARTPPT + 1
      }
      
    }else{
      # Grab regular Prop Tax 
      PropTax0 <- data.frame(na.omit(MonthlyFS[c(10:(nrow(MonthlyFS))),c(1,45)]))  # Grab Column
      
      # Narrow down to 2016 if neeeded
      See<- PropTax0[!grepl("2010", PropTax0[,1]),]
      See<- See[!grepl("2011", See[,1]),]
      See<- See[!grepl("2012", See[,1]),]
      See<- See[!grepl("2013", See[,1]),]
      See<- See[!grepl("2014", See[,1]),]
      See<- See[!grepl("2015", See[,1]),]
      
      PropTax0<- data.frame(na.omit(See))
      
      
      
      rownames(PropTax0)<- PropTax0[,1]
      PropTax<- PropTax0[2]
      

      # First Sheet  
      cPropTax <- data.frame(which(colnames(Property_Tax1)%in% rownames(PropTax)))
      
      wPropTax<-1
      for(wPropTax in 1:nrow(cPropTax)){
        value<- PropTax[wPropTax,1]
        Property_Tax1[Count,cPropTax[wPropTax,1]]<- value
        
        wPropTax<-wPropTax+1
      }
      
      # Second Sheet
      dPropTax<- data.frame(which(colnames(Property_Tax2) %in% rownames(PropTax)))
      
      wwPropTax <- nrow(PropTax) - nrow(cPropTax)
      STARTPropTax<- 1
      
      for(STARTPropTax in 1:nrow(dPropTax)){
        value<- PropTax[wwPropTax,1]
        Property_Tax2[Count,dPropTax[STARTPropTax,1]]<- value
        
        wwPropTax<-wwPropTax+1
        STARTPropTax<- STARTPropTax + 1
      }
      
      
    }
    
############### Prop Tax ABOVE ###########################
    
    
############### Site Lease##############################  
    if("Site Lease" %in% MonthlyFS[8,27]){
      SiteLease0 <- data.frame(na.omit(MonthlyFS[c(10:(nrow(MonthlyFS))),c(1,27)]))  # Grab Column
      
      # Narrow down to 2016 if neeeded
      See<- SiteLease0[!grepl("2010", SiteLease0[,1]),]
      See<- See[!grepl("2011", See[,1]),]
      See<- See[!grepl("2012", See[,1]),]
      See<- See[!grepl("2013", See[,1]),]
      See<- See[!grepl("2014", See[,1]),]
      See<- See[!grepl("2015", See[,1]),]
      
      SiteLease0<- data.frame(na.omit(See))
      
      rownames(SiteLease0)<- SiteLease0[,1]
      SiteLease<- SiteLease0[2]
      
      
      # First Sheet  
      cSiteLease <- data.frame(which(colnames(Site_Lease1)%in% rownames(SiteLease)))
      
      wSiteLease<-1
      for(wSiteLease in 1:nrow(cSiteLease)){
        value<- SiteLease[wSiteLease,1]
        Site_Lease1[Count,cSiteLease[wSiteLease,1]]<- value
        
        wSiteLease<-wSiteLease+1
      }
      
      # Second Sheet
      dSiteLease<- data.frame(which(colnames(Site_Lease2) %in% rownames(SiteLease)))
      
      wwSiteLease <- nrow(SiteLease) - nrow(cSiteLease)
      STARTSiteLease<- 1
      
      for(STARTSiteLease in 1:nrow(dSiteLease)){
        value<- SiteLease[wwSiteLease,1]
        Site_Lease2[Count,dSiteLease[STARTSiteLease,1]]<- value
        
        wwSiteLease<-wwSiteLease+1
        STARTSiteLease<- STARTSiteLease + 1
      }
      
      
    }else {
      #### If Site Lease doesnt  exists, make the whole Pull 0 for the sheet
      SiteLeaseNone0 <- data.frame(na.omit(MonthlyFS[c(10:(nrow(MonthlyFS))),c(1,27)])) # Grab Column
      
      # Narrow down to 2016 if neeeded
      See<- SiteLeaseNone0[!grepl("2010", SiteLeaseNone0[,1]),]
      See<- See[!grepl("2011", See[,1]),]
      See<- See[!grepl("2012", See[,1]),]
      See<- See[!grepl("2013", See[,1]),]
      See<- See[!grepl("2014", See[,1]),]
      See<- See[!grepl("2015", See[,1]),]
      
      SiteLeaseNone0<- data.frame(na.omit(See))
      
      rownames(SiteLeaseNone0)<- SiteLeaseNone0[,1]
      SiteLeaseNone<- SiteLeaseNone0[2]
      
      
      
      
      # First Sheet  
      cSiteLeaseNone <- data.frame(which(colnames(Site_Lease1)%in% rownames(SiteLeaseNone)))
      
      wSiteLeaseNone<-1
      for(wSiteLeaseNone in 1:nrow(cSiteLeaseNone)){
        value<- SiteLeaseNone[wSiteLeaseNone,1]
        Site_Lease1[Count,cSiteLeaseNone[wSiteLeaseNone,1]]<- value
        
        wSiteLeaseNone<-wSiteLeaseNone+1
      }
      
      # Second Sheet
      dSiteLeaseNone<- data.frame(which(colnames(Site_Lease2) %in% rownames(SiteLeaseNone)))
      
      wwSiteLeaseNone <- nrow(SiteLeaseNone) - nrow(cSiteLeaseNone)
      STARTSiteLeaseNone<- 1
      
      for(STARTSiteLeaseNone in 1:nrow(dSiteLeaseNone)){
        value<- SiteLeaseNone[wwSiteLeaseNone,1]
        Site_Lease2[Count,dSiteLeaseNone[STARTSiteLeaseNone,1]]<- value
        
        wwSiteLeaseNone<-wwSiteLeaseNone+1
        STARTSiteLeaseNone<- STARTSiteLeaseNone + 1
      }
      
      
    }
    
   
############### SITE LEASE ABOVE ###########################
    
  
  ## overall count
  
  a<- a+1
  Count<- Count + 1
}




############## Temporary fix for blanks #################
# Currently just running for Walmart

column<- which(Sheet1_Temp$`Sun Financial Projects` %in% "SSACA1 R")

blank<- 1
for(blank in 1:(ncol(Sheet1_Temp)-5)){
  Grant_Rebates1[column,blank+5]<- ''
  Grant_Rebates2[column,blank+5]<- ''
  Lease_Payments1[column,blank+5]<- ''
  Lease_Payments2[column,blank+5]<- ''
  SLB_Proceeds1[column,blank+5]<- ''
  SLB_Proceeds2[column,blank+5]<- ''
  SREC_Rev1[column,blank+5]<- ''
  SREC_Rev2[column,blank+5]<- ''
  Property_Tax1[column,blank+5]<- ''
  Property_Tax2[column,blank+5]<- ''
  Site_Lease1[column,blank+5]<- ''
  Site_Lease2[column,blank+5]<- ''
  
  blank<- blank+1
}


############################################################################






# Too many columns :( WriteXLS::WriteXLS(SLB_Proceeds1, "test.xls", SheetNames = "SLB")



############ Write the CVS Files ############################

write.table(Grant_Rebates1, file = "GrantRebates 1.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")
write.table(Grant_Rebates2, file = "GrantRebates 2.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")


write.table(Lease_Payments1, file = "LeasePayments 1.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")
write.table(Lease_Payments2, file = "LeasePayments 2.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")

write.table(SLB_Proceeds1, file = "SLBProceeds 1.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")
write.table(SLB_Proceeds2, file = "SLBProceeds 2.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")

write.table(SREC_Rev1, file = "SREC Rev 1.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")
write.table(SREC_Rev2, file = "SREC Rev 2.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")


write.table(Property_Tax1, file = "PropertyTax 1.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")
write.table(Property_Tax2, file = "PropertyTax 2.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")

write.table(Site_Lease1, file = "SiteLease 1.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")
write.table(Site_Lease2, file = "SiteLease 2.csv",row.names=FALSE, na="",
            col.names=TRUE, sep=",")

