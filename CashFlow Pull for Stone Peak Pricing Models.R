# Cash Flow Recuglar Cube upload 
# Data Mining Stone Peak Pricing Models
# Nichole Freeman
# Start Date: 2.05.2018
# Completed Date: 2.05.2018  (additions to come for additional pulls)
# Last updated: 2.05.2018  



# the purpose of this program is to grab data from a giant excel sheet
# that has tons of references that takes to long manually look up
# I also want to add dates that aren't there so all of the models
# start at 012016
# Some do not have certain attributes, so I'll have to make conditions


# Directions:
# Press Cash Flow Template Twice
# PVI Upload once
# Then Pick the right site as it pops up on screen

###################################################################
################### Grab Cash Flows from SPP Model ################

require(readlx)
require(lubridate)
require(janitor)

# Create Shells of sheets
# Click Cash Flow Template twice
Sheet1_Temp<- readxl::read_xlsx(file.choose(), col_names=TRUE, sheet = "1216 to 0237")
Sheet2_Temp<- readxl::read_xlsx(file.choose(), col_names=TRUE, sheet = "0337 to 1250")

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


# start the overall loop

Count<- 1
End<- nrow(Sheet1_Temp)
a<- 1

for (a in 1:End){
  print(Grant_Rebates1[Count,5])
  
  #Okay, process to convert dates first:
  MonthlyFS<- readxl::read_xlsx(file.choose(), col_names=TRUE, sheet = "Monthly FS")
  dates<- MonthlyFS[(9:800),1]
  dates<- data.frame(na.omit(dates))
  
  Num<- nrow(dates)
  i<-1
  rowstart<- 9
  
  for(i in 1:Num){
    Convert <- janitor::excel_numeric_to_date(as.numeric(dates[i,1]), date_system = "modern")
    form<- format.Date(Convert, '%m%Y')
    MonthlyFS[rowstart,1]<- form
    
    i<- i+1
    rowstart<- rowstart + 1
  }
  
################### Grant Rebates ############################
  GR0 <- data.frame(MonthlyFS[c(9:nrow(MonthlyFS)),c(1,33)])  # Grab Column
  
  #Narrow down to 012016
  See<- GR0[!grepl("2011", GR0[,1]),]
  See<- See[!grepl("2010", See[,1]),]
  See<- See[!grepl("2012", See[,1]),]
  See<- See[!grepl("2013", See[,1]),]
  See<- See[!grepl("2014", See[,1]),]
  See<- See[!grepl("2015", See[,1]),]
  
  GR<- data.frame(na.omit(See)) # Now here's the cleaned data that we want to put on sheet

  # since they start at different dates and we want them to start in 012016
  # we have to add columns

  # below is to add columns if started later than 2016
  seventeen<- c("012017","022017","032017","042017","052017","062017",
                "072017","082017","092017","102017","112017","122017")
  newrow17<- data.frame(seventeen,0)
  colnames(newrow17)<- colnames(GR)
  
  sixteen<- c("012016","022016","032016","042016","052016","062016",
              "072016","082016","092016","102016","112016","122016")
  newrow16<- data.frame(sixteen,0)
  colnames(newrow16)<- colnames(GR)
  
  
    #add 2017 First
    alpha0<- 12
    for (alpha0 in 12:1){
      if(!newrow17[alpha0,1] %in% GR[,1]){
        add0<-newrow17[alpha0,]
        GR<- rbind(add0,GR)
      }
      alpha0<- alpha0-1
    }
    
    #the 2016 so it is at the top
    alpha<- 12
    for (alpha in 12:1){
      if(!newrow16[alpha,1] %in% GR[,1]){
        add<-newrow16[alpha,]
        GR<- rbind(add,GR)
      }
      alpha<- alpha-1
    }
    
  j<- 1
  colG<- 6
# for Sheet 1 
  for (j in 1:254){
    Grant_Rebates1[Count,colG]<-GR[j,2]
    j<- j +1
    colG<- colG + 1
  }

# for sheet 2 
  topGR<- 1 
  jj<- 255
  colGG<- 6
  
  for (topGr in 1:166){
    Grant_Rebates2[Count,colGG]<-GR[jj,2]
    jj<- jj +1
    colGG<- colGG + 1
    topGr<- topGr +1
  }

#############################GR ABOVE #####################################
  
##################LEASE PAYMENTS BELOW ####################################
  LP0 <- data.frame(MonthlyFS[c(9:nrow(MonthlyFS)),c(1,51)])  # Grab Column
  
  #Narrow down to 012016
  SeeLP<- LP0[!grepl("2011", LP0[,1]),]
  SeeLP<- SeeLP[!grepl("2012", SeeLP[,1]),]
  SeeLP<- SeeLP[!grepl("2010", SeeLP[,1]),]
  SeeLP<- SeeLP[!grepl("2013", SeeLP[,1]),]
  SeeLP<- SeeLP[!grepl("2014", SeeLP[,1]),]
  SeeLP<- SeeLP[!grepl("2015", SeeLP[,1]),]

  
  LP<- data.frame(na.omit(SeeLP)) # Now here's the cleaned data that we want to put on sheet
  
  # since they start at different dates and we want them to start in 012016
  # we have to add columns
  # below is to add columns if started later than 2016
  seventeen<- c("012017","022017","032017","042017","052017","062017",
                "072017","082017","092017","102017","112017","122017")
  newrow17<- data.frame(seventeen,0)
  colnames(newrow17)<- colnames(LP)
  
  sixteen<- c("012016","022016","032016","042016","052016","062016",
              "072016","082016","092016","102016","112016","122016")
  newrow16<- data.frame(sixteen,0)
  colnames(newrow16)<- colnames(LP)
  
    #add 2017 First
  bravo0<- 12
  for (bravo0 in 12:1){
    if(!newrow17[bravo0,1] %in% LP[,1]){
      add0<-newrow17[bravo0,]
      LP<- rbind(add0,LP)
    }
    bravo0<- bravo0-1
  }
  
  #the 2016 so it is at the top
  bravo<- 12
  for (bravo in 12:1){
    if(!newrow16[bravo,1] %in% LP[,1]){
      add<-newrow16[bravo,]
      LP<- rbind(add,LP)
    }
    bravo<- bravo-1
  }
  
  
  
  L<- 1
  colL<- 6
  # for Sheet 1 
  for (L in 1:254){
    Lease_Payments1[Count,colL]<-LP[L,2]
    L<- L +1
    colL<- colL + 1
  }
  
  # for sheet 2 
  LL<- 255
  colLL<- 6
  topLL<- 1
  
  for (topLL in 1:166){
    Lease_Payments2[Count,colLL]<-LP[LL,2]
    LL<- LL +1
    colLL<- colLL + 1
    topLL<- topLL +1 
  }
#########################LEASE PAY ABOVE ##########################

#########################SLB PROCEEDS BELOW #######################
  SLB0 <- data.frame(MonthlyFS[c(9:nrow(MonthlyFS)),c(1,35)])  # Grab Column
  
  #Narrow down to 012016
  SeeSLB<- SLB0[!grepl("2011", SLB0[,1]),]
  SeeSLB<- SeeSLB[!grepl("2012", SeeSLB[,1]),]
  SeeSLB<- SeeSLB[!grepl("2010", SeeSLB[,1]),]
  SeeSLB<- SeeSLB[!grepl("2013", SeeSLB[,1]),]
  SeeSLB<- SeeSLB[!grepl("2014", SeeSLB[,1]),]
  SeeSLB<- SeeSLB[!grepl("2015", SeeSLB[,1]),]

  
  SLB<- data.frame(na.omit(SeeSLB)) # Now here's the cleaned data that we want to put on sheet
  
  # since they start at different dates and we want them to start in 012016
  # we have to add columns
  
  # below is to add columns if started later than 2016
  seventeen<- c("012017","022017","032017","042017","052017","062017",
                "072017","082017","092017","102017","112017","122017")
  newrow17<- data.frame(seventeen,0)
  colnames(newrow17)<- colnames(SLB)
  
  sixteen<- c("012016","022016","032016","042016","052016","062016",
              "072016","082016","092016","102016","112016","122016")
  newrow16<- data.frame(sixteen,0)
  colnames(newrow16)<- colnames(SLB)
  
  
  
  #add 2017 First
  charlie0<- 12
  for (charlie0 in 12:1){
    if(!newrow17[charlie0,1] %in% SLB[,1]){
      add0<-newrow17[charlie0,]
      SLB<- rbind(add0,SLB)
    }
    charlie0<- charlie0-1
  }
  
  #the 2016 so it is at the top
  charlie<- 12
  for (charlie in 12:1){
    if(!newrow16[charlie,1] %in% SLB[,1]){
      add<-newrow16[charlie,]
      SLB<- rbind(add,SLB)
    }
    charlie<- charlie-1
  }
  
  
  
  
  S<- 1
  colS<- 6
  # for Sheet 1 
  for (S in 1:254){
    SLB_Proceeds1[Count,colS]<-SLB[S,2]
    S<- S +1
    colS<- colS + 1
  }
  
  # for sheet 2 
  SS<- 255
  colSS<- 6
  topSS<- 1
  
  for (topSS in 1:166){
    SLB_Proceeds2[Count,colSS]<-SLB[SS,2]
    SS<- SS +1
    colSS<- colSS + 1
    topSS<- topSS + 1 
    
  }
#################################SLB PROCEEDS ABOVE############
  
####################SREC REV BELOW ############################
  SREC0 <- data.frame(MonthlyFS[c(9:nrow(MonthlyFS)),c(1,41)])  # Grab Column
  
  #Narrow down to 012016
  SeeSR<- SREC0[!grepl("2011", SREC0[,1]),]
  SeeSR<- SeeSR[!grepl("2012", SeeSR[,1]),]
  SeeSR<- SeeSR[!grepl("2010", SeeSR[,1]),]
  SeeSR<- SeeSR[!grepl("2013", SeeSR[,1]),]
  SeeSR<- SeeSR[!grepl("2014", SeeSR[,1]),]
  SeeSR<- SeeSR[!grepl("2015", SeeSR[,1]),]
  
  SREC<- data.frame(na.omit(SeeSR)) # Now here's the cleaned data that we want to put on sheet
  
  # since they start at different dates and we want them to start in 012016
  # we have to add columns
  
  # below is to add columns if started later than 2016
  seventeen<- c("012017","022017","032017","042017","052017","062017",
                "072017","082017","092017","102017","112017","122017")
  newrow17<- data.frame(seventeen,0)
  colnames(newrow17)<- colnames(SREC)
  
  sixteen<- c("012016","022016","032016","042016","052016","062016",
              "072016","082016","092016","102016","112016","122016")
  newrow16<- data.frame(sixteen,0)
  colnames(newrow16)<- colnames(SREC)
  
  
    #add 2017 First
  delta0<- 12
  for (delta0 in 12:1){
    if(!newrow17[delta0,1] %in% SREC[,1]){
      add0<-newrow17[delta0,]
      SREC<- rbind(add0,SREC)
    }
    delta0<- delta0-1
  }
  
  #the 2016 so it is at the top
  delta<- 12
  for (delta in 12:1){
    if(!newrow16[delta,1] %in% SREC[,1]){
      add<-newrow16[delta,]
      SREC<- rbind(add,SREC)
    }
    delta<- delta-1
  }
  
  
  
  SR<- 1
  colSR<- 6
  # for Sheet 1 
  for (SR in 1:254){
    SREC_Rev1[Count,colSR]<-SREC[SR,2]
    SR<- SR +1
    colSR<- colSR + 1
  }
  
  # for sheet 2 
  SRR<- 255
  colSRR<- 6
  topSR<- 1
  
  for (topSR in 1:166){
    SREC_Rev2[Count,colSRR]<-SREC[SRR,2]
    SRR<- SRR +1
    colSRR<- colSRR + 1
    topSR<- topSR +1
  }
  
#############################SREC REV ABOVE #####################################

  
######################Property Tax BELOW ############################
  
  # Okay, so Property tax also has two columns based off a condition
  # so here we go makin it!!
  
  if("PPT" %in% MonthlyFS[8,49]){
### Now get the PPT column
      PPT0 <- data.frame(MonthlyFS[c(9:nrow(MonthlyFS)),c(1,49)])  # Grab Column
      
      #Narrow down to 012016
      SeePPT<- PPT0[!grepl("2011", PPT0[,1]),]
      SeePPT<- SeePPT[!grepl("2010", SeePPT[,1]),]
      SeePPT<- SeePPT[!grepl("2012", SeePPT[,1]),]
      SeePPT<- SeePPT[!grepl("2013", SeePPT[,1]),]
      SeePPT<- SeePPT[!grepl("2014", SeePPT[,1]),]
      SeePPT<- SeePPT[!grepl("2015", SeePPT[,1]),]
      
      
      PPT<- data.frame(na.omit(SeePPT)) # Now here's the cleaned data that we want 
      # to put on sheet
      
      # since they start at different dates and we want them to start in 012016
      # we have to add columns
      
      # below is to add columns if started later than 2016
      seventeen<- c("012017","022017","032017","042017","052017","062017",
                    "072017","082017","092017","102017","112017","122017")
      newrow17<- data.frame(seventeen,0)
      colnames(newrow17)<- colnames(PPT)
      
      sixteen<- c("012016","022016","032016","042016","052016","062016",
                  "072016","082016","092016","102016","112016","122016")
      newrow16<- data.frame(sixteen,0)
      colnames(newrow16)<- colnames(PPT)
      
      #add 2017 First
      golf0<- 12
      for (golf0 in 12:1){
        if(!newrow17[golf0,1] %in% PPT[,1]){
          add0<-newrow17[golf0,]
          PPT<- rbind(add0,PPT)
        }
        golf0<- golf0-1
      }
      
      #the 2016 so it is at the top
      golf<- 12
      for (golf in 12:1){
        if(!newrow16[golf,1] %in% PPT[,1]){
          add<-newrow16[golf,]
          PPT<- rbind(add,PPT)
        }
        golf<- golf-1
      }
      
      
      PPT_counter<- 1
      colPPT_counter<- 6
      # for Sheet 1 
      for (PPT_counter in 1:254){
        Property_Tax1[Count,colPPT_counter]<-PPT[PPT_counter,2]
        PPT_counter<- PPT_counter +1
        colPPT_counter<- colPPT_counter + 1
      }
      
      # for sheet 2 
      PPT_counterT<- 255
      colPPT_counterT<- 6
      topPPT_counter<- 1
      
      for (topPPT_counter in 1:166){
        Property_Tax2[Count,colPPT_counterT]<-PPT[PPT_counterT,2]
        PPT_counterT<- PPT_counterT +1
        colPPT_counterT<- colPPT_counterT + 1
        topPPT_counter<- topPPT_counter +1
      }
      
  } else {
    
    # Grab regular Prop Tax 
      PropTax0 <- data.frame(MonthlyFS[c(9:nrow(MonthlyFS)),c(1,45)])  # Grab Column
      
      #Narrow down to 012016
      SeePT<- PropTax0[!grepl("2011", PropTax0[,1]),]
      SeePT<- SeePT[!grepl("2010", SeePT[,1]),]
      SeePT<- SeePT[!grepl("2012", SeePT[,1]),]
      SeePT<- SeePT[!grepl("2013", SeePT[,1]),]
      SeePT<- SeePT[!grepl("2014", SeePT[,1]),]
      SeePT<- SeePT[!grepl("2015", SeePT[,1]),]
      
      
      PropTax<- data.frame(na.omit(SeePT)) # Now here's the cleaned data that we want 
      # to put on sheet
      
      # since they start at different dates and we want them to start in 012016
      # we have to add columns
      
      # below is to add columns if started later than 2016
      seventeen<- c("012017","022017","032017","042017","052017","062017",
                    "072017","082017","092017","102017","112017","122017")
      newrow17<- data.frame(seventeen,0)
      colnames(newrow17)<- colnames(PropTax)
      
      sixteen<- c("012016","022016","032016","042016","052016","062016",
                  "072016","082016","092016","102016","112016","122016")
      newrow16<- data.frame(sixteen,0)
      colnames(newrow16)<- colnames(PropTax)
      
      #add 2017 First
      echo0<- 12
      for (echo0 in 12:1){
        if(!newrow17[echo0,1] %in% PropTax[,1]){
          add0<-newrow17[echo0,]
          PropTax<- rbind(add0,PropTax)
        }
        echo0<- echo0-1
      }
      
      #the 2016 so it is at the top
      echo<- 12
      for (echo in 12:1){
        if(!newrow16[echo,1] %in% PropTax[,1]){
          add<-newrow16[echo,]
          PropTax<- rbind(add,PropTax)
        }
        echo<- echo-1
      }
      
      
      PT<- 1
      colPT<- 6
      # for Sheet 1 
      for (PT in 1:254){
        Property_Tax1[Count,colPT]<-PropTax[PT,2]
        PT<- PT +1
        colPT<- colPT + 1
      }
      
      # for sheet 2 
      PTT<- 255
      colPTT<- 6
      topPT<- 1
      
      for (topPT in 1:166){
        Property_Tax2[Count,colPTT]<-PropTax[PTT,2]
        PTT<- PTT +1
        colPTT<- colPTT + 1
        topPT<- topPT +1
      } # end of else statement 
    
    
  } # end of prop tax all 
  
    
    
  
  
#############################Property Tax ABOVE #####################################
  
############################Site Lease Below -- with conditions ####################

# Okay, so site leases are different than any of these
# because not all of them have it 

# so lets set the condition first -- should run if in program

  if("Site Lease" %in% MonthlyFS[8,27]){
    
    SiteLease0 <- data.frame(MonthlyFS[c(9:nrow(MonthlyFS)),c(1,27)])  # Grab Column
    
    #Narrow down to 012016
    SeeSite<- SiteLease0[!grepl("2011", SiteLease0[,1]),]
    SeeSite<- SeeSite[!grepl("2010", SeeSite[,1]),]
    SeeSite<- SeeSite[!grepl("2012", SeeSite[,1]),]
    SeeSite<- SeeSite[!grepl("2013", SeeSite[,1]),]
    SeeSite<- SeeSite[!grepl("2014", SeeSite[,1]),]
    SeeSite<- SeeSite[!grepl("2015", SeeSite[,1]),]
    
    
    SiteLease<- data.frame(na.omit(SeeSite)) # Now here's the cleaned data that we want 
    # to put on sheet
    
    # since they start at different dates and we want them to start in 012016
    # we have to add columns
    
    # below is to add columns if started later than 2016
    seventeen<- c("012017","022017","032017","042017","052017","062017",
                  "072017","082017","092017","102017","112017","122017")
    newrow17<- data.frame(seventeen,0)
    colnames(newrow17)<- colnames(SiteLease)
    
    sixteen<- c("012016","022016","032016","042016","052016","062016",
                "072016","082016","092016","102016","112016","122016")
    newrow16<- data.frame(sixteen,0)
    colnames(newrow16)<- colnames(SiteLease)
    
    #add 2017 First
    foxtrot0<- 12
    for (foxtrot0 in 12:1){
      if(!newrow17[foxtrot0,1] %in% SiteLease[,1]){
        add0<-newrow17[foxtrot0,]
        SiteLease<- rbind(add0,SiteLease)
      }
      foxtrot0<- foxtrot0-1
    }
    
    #the 2016 so it is at the top
    foxtrot<- 12
    for (foxtrot in 12:1){
      if(!newrow16[foxtrot,1] %in% SiteLease[,1]){
        add<-newrow16[foxtrot,]
        SiteLease<- rbind(add,SiteLease)
      }
      foxtrot<- foxtrot-1
    }
    
    
    SL<- 1
    colSL<- 6
    # for Sheet 1 
    for (SL in 1:254){
      Site_Lease1[Count,colSL]<-SiteLease[SL,2]
      SL<- SL +1
      colSL<- colSL + 1
    }
    
    # for sheet 2 
    SLL<- 255
    colSLL<- 6
    topSL<- 1
    
    for (topSL in 1:166){
      Site_Lease2[Count,colSLL]<-SiteLease[SLL,2]
      SLL<- SLL +1
      colSLL<- colSLL + 1
      topSL<- topSL +1
    }
  } else {
    #### If Site Lease exists, make the whole Pull 0 for the sheet
      SiteLeaseNone0 <- data.frame(MonthlyFS[c(9:nrow(MonthlyFS)),c(1,31)])
      
      #Narrow down to 012016
      SeeSiteNone<- SiteLeaseNone0[!grepl("2011", SiteLeaseNone0[,1]),]
      SeeSiteNone<- SeeSiteNone[!grepl("2010", SeeSiteNone[,1]),]
      SeeSiteNone<- SeeSiteNone[!grepl("2012", SeeSiteNone[,1]),]
      SeeSiteNone<- SeeSiteNone[!grepl("2013", SeeSiteNone[,1]),]
      SeeSiteNone<- SeeSiteNone[!grepl("2014", SeeSiteNone[,1]),]
      SeeSiteNone<- SeeSiteNone[!grepl("2015", SeeSiteNone[,1]),]
      
      
      SiteLeaseNone<- SeeSiteNone 
      SiteLeaseNone[,2]<- 0    # make them all zeroes instead of N/A
      
      # Now here's the cleaned data that we want 
      # to put on sheet
      
      # since they start at different dates and we want them to start in 012016
      # we have to add columns
      
      # below is to add columns if started later than 2016
      seventeen<- c("012017","022017","032017","042017","052017","062017",
                    "072017","082017","092017","102017","112017","122017")
      newrow17<- data.frame(seventeen,0)
      colnames(newrow17)<- colnames(SiteLeaseNone)
      
      sixteen<- c("012016","022016","032016","042016","052016","062016",
                  "072016","082016","092016","102016","112016","122016")
      newrow16<- data.frame(sixteen,0)
      colnames(newrow16)<- colnames(SiteLeaseNone)
      
      #add 2017 First
      foxtrot0<- 12
      for (foxtrot0 in 12:1){
        if(!newrow17[foxtrot0,1] %in% SiteLeaseNone[,1]){
          add0<-newrow17[foxtrot0,]
          SiteLeaseNone<- rbind(add0,SiteLeaseNone)
        }
        foxtrot0<- foxtrot0-1
      }
      
      #the 2016 so it is at the top
      foxtrot<- 12
      for (foxtrot in 12:1){
        if(!newrow16[foxtrot,1] %in% SiteLeaseNone[,1]){
          add<-newrow16[foxtrot,]
          SiteLeaseNone<- rbind(add,SiteLeaseNone)
        }
        foxtrot<- foxtrot-1
      }
      
      SL<- 1
      colSL<- 6
      # for Sheet 1 
      for (SL in 1:254){
        Site_Lease1[Count,colSL]<-SiteLeaseNone[SL,2]
        SL<- SL +1
        colSL<- colSL + 1
      }
      
      # for sheet 2 
      SLL<- 255
      colSLL<- 6
      topSL<- 1
      
      for (topSL in 1:166){
        Site_Lease2[Count,colSLL]<-SiteLeaseNone[SLL,2]
        SLL<- SLL +1
        colSLL<- colSLL + 1
        topSL<- topSL +1
      }
      
    }
    
########################### Last Part of the Count below######################
  
  
  Count<- Count + 1
  a< a + 1 
  
} # This is immediate end













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


















#######################################################
#Extra code just incase we need to grab it later
# company_name<- data.frame(matrix(PVI[1,2], 1,1))
# facility_name<- data.frame(matrix(PVI[2,2], 1,1))
# corp_ID<- data.frame(matrix(PVI[3,2], 1,1))
# location<- data.frame(matrix(PVI[24,5], 1,1))
# size_in_kwh<- data.frame(matrix(PVI[26,5], 1,1))
# inverter_rate_cost_perW_<- data.frame(matrix(PVI[31,5], 1,1))
# property_tax_selection<- data.frame(matrix(PVI[282,5], 1,1))
# inverter_reserve_OpExp<- data.frame(matrix(PVI[284,5], 1,1)) 

#Commence_Date00<- as.numeric(PVICF[81,5])
#Commence_Date0<- as.Date(Commence_Date00, origin = "1899-12-30")
#Commence_Date<- format.Date(Commence_Date0, '%m%Y')
#dates<- str_replace_all(dates, "[[:punct:]]", "")
#dates<- str_replace_all(dates, "c", "")
#dates<- as.vector(dates)
              