
## R web scraping for Recycle Data

library(rvest)
library(RCurl)

require(httr)
require(XML)
library(stringr)


basePage <- "http://www.calrecycle.ca.gov/LGCentral/Reports/DiversionProgram"

year =2007
jurID = 169

urlFun <- function(year,jurID){
  
  url <- paste0(basePage,"/JurisdictionDiversionDetail.aspx?JurisdictionID=",jurID,"&Year=",year)
  doc = htmlParse(url)
  tableNodes = getNodeSet(doc, "//table")
  print(tableNodes)
  tb1 = readHTMLTable(tableNodes[[1]],header=FALSE)
  tb2 = readHTMLTable(tableNodes[[2]],header=FALSE)
  tb3 = readHTMLTable(tableNodes[[3]],header=FALSE)
  
  # Table 1 Variables
  jur <- as.character(tb1$V2[1])            #Jurisdiction
  county <- as.character(tb1$V2[2])         #County
  yr <- year                                #Year
  
  # Table 2 Variables
  yrdis <- as.numeric(as.character(sub(",","",tb2$V2[1])))        #Reporting-Year Disposal Amount (tons)
  redcred <- as.numeric(as.character(sub(",","",tb2$V2[2])))         #Disposal Reduction Credits (Reported)
  diswaste <- as.numeric(as.character(sub(",","",tb2$V2[3])))        #Disaster Waste (tons)
  medwaste <- as.numeric(as.character(sub(",","",tb2$V2[4])))        #Medical Waste (tons)
  divfasreswaste <- as.numeric(as.character(sub(",","",tb2$V2[5])))  #Regional Diversion Facility Residual Waste (tons)
  cdwaste <- as.numeric(as.character(sub(",","",tb2$V2[6] )))         #C&D Waste (tons)
  outofstateexport <- as.numeric(as.character(sub(",","",tb2$V2[7]))) #Out-of-State Export (Diverted)
  class2waste <- as.numeric(as.character(sub(",","",tb2$V2[8])))     #Class II Waste (tons)
  otherdisposal <-as.numeric(as.character(sub(",","",tb2$V2[9])))    #Other Disposal Amount (tons)
  totaldispred <- as.numeric(as.character(sub(",","",tb2$V2[11])))    #Total Disposal Reduction Credit Amount (tons)
  totaladjdisp <- as.numeric(as.character(sub(",","",tb2$V2[12])))    #Total Adjusted Reporting-Year Disposal Amount (tons)
  transwaste <- as.numeric(as.character(sub(",","",tb2$V2[13])))     #Reporting-Year Transformation Waste (tons)
  population <- as.numeric(as.character(sub(",","",tb2$V2[14])))      #Reporting-Year Population
  employment <- as.numeric(as.character(sub(",","",tb2$V2[15])))      #Reporting-Year Employment
  
  
  # Table 3 Variables
  dispRateTransPopTar <- as.numeric(as.character(tb3$V2[3]))         #Disposal Rate without Transformation (pounds/person/day): Population Target
  transRateTransPopTar <- as.numeric(as.character(tb3$V2[4]))         #Transformation Rate (pounds/person/day): Population Target
  calcDispRatePopTar <- as.numeric(as.character(tb3$V2[5]))          #Calculated Disposal Rate (pounds/person/day) Population Target
  
  dispRateTransPopAnnual <- as.numeric(as.character(tb3$V3[3]))       #Disposal Rate without Transformation (pounds/person/day): Population Annual
  transRateTransPopAnnual <- as.numeric(as.character(tb3$V3[4]))      #Transformation Rate (pounds/person/day): Population Annual
  calcDispRatePopAnnual <- as.numeric(as.character(tb3$V3[5]))        #Calculated Disposal Rate (pounds/person/day) Population Annual
  
  dispRateTransEmpTar <- as.numeric(as.character(tb3$V4[3]))          #Disposal Rate without Transformation (pounds/person/day): Employment Target 
  transRateTransEmpTar <- as.numeric(as.character(tb3$V4[4]))         #Transformation Rate (pounds/person/day): Employment Target
  calcDispRateEmpTar <- as.numeric(as.character(tb3$V4[5]))           #Calculated Disposal Rate (pounds/person/day) Employment Target
  
  dispRateTransEmpAnnual <- as.numeric(as.character(tb3$V5[3]))       #Disposal Rate without Transformation (pounds/person/day): Employment Annual
  transRateTransEmpAnnual <- as.numeric(as.character(tb3$V5[4]))      #Transformation Rate (pounds/person/day): Employment Annual
  calcDispRateEmpAnnual <- as.numeric(as.character(tb3$V5[5]))        #Calculated Disposal Rate (pounds/person/day) Employment Annual
  
  res <- list(jur,county,yr,yrdis,redcred,diswaste,medwaste,divfasreswaste,cdwaste,outofstateexport,
              class2waste,otherdisposal,totaldispred,totaladjdisp,transwaste,population,employment,
              dispRateTransPopTar,transRateTransPopTar,calcDispRatePopTar,dispRateTransPopAnnual,transRateTransPopAnnual,calcDispRatePopAnnual,
              dispRateTransEmpTar,transRateTransEmpTar,calcDispRateEmpTar,dispRateTransEmpAnnual,transRateTransEmpAnnual,calcDispRateEmpAnnual
              )
  return(res)
}

# first method
## USE MAPPLY and REDUCE
yrs <- 2007
juris <- 1:10

years <-as.vector(mapply(rep,yrs,10))
jurs <-as.vector(rep(juris,1))
ress<-mapply(urlFun,years,jurs)


# second method 
df <- c()
for (i in 1:10){
  
  df<- cbind(df,urlFun(year=2007,jurID = i))

}

df2007 <- t(df)
colnames(df2007) <- c("Jurisdiction","County","Year","Reporting-Year-Disposal-Amount","Disposal Reduction Credits","Disaster Waste",
                      "Medical Waste","Regional Diversion Facility Residual Waste","C&D Waste","Out-of-State Export","Class II Waste",
                      "Other Disposal Amount","Total Disposal Reduction Credit Amount","Total Adjusted Reporting-Year Disposal Amount",
                      "Reporting-Year Transformation Waste","Reporting-Year Population",
                      "Reporting-Year Employment","Disposal Rate without Transformation (pounds/person/day): Population Target",
                      "Transformation Rate (pounds/person/day): Population Target","Calculated Disposal Rate (pounds/person/day) Population Target",
                      "Disposal Rate without Transformation (pounds/person/day): Population Annual","Transformation Rate (pounds/person/day): Population Annual",
                      "Calculated Disposal Rate (pounds/person/day) Population Annual","Disposal Rate without Transformation (pounds/person/day): Employment Target",
                      "Transformation Rate (pounds/person/day): Employment Target","Calculated Disposal Rate (pounds/person/day) Employment Target",
                      "Disposal Rate without Transformation (pounds/person/day): Employment Annual","Transformation Rate (pounds/person/day): Employment Annual",
                      "Calculated Disposal Rate (pounds/person/day) Employment Annual"
                      )

write.csv(df2007,file="Dropbox/NewMet Project/Diversion2007.csv")






df <- c()
for (i in 1:700){
  
  df<- cbind(df,urlFun(year=2008,jurID = i))
  
}

df2008 <- t(df)
colnames(df2008) <- c("Jurisdiction","County","Year","Reporting-Year-Disposal-Amount","Disposal Reduction Credits","Disaster Waste",
                      "Medical Waste","Regional Diversion Facility Residual Waste","C&D Waste","Out-of-State Export","Class II Waste",
                      "Other Disposal Amount","Total Disposal Reduction Credit Amount","Total Adjusted Reporting-Year Disposal Amount",
                      "Reporting-Year Transformation Waste","Reporting-Year Population",
                      "Reporting-Year Employment","Disposal Rate without Transformation (pounds/person/day): Population Target",
                      "Transformation Rate (pounds/person/day): Population Target","Calculated Disposal Rate (pounds/person/day) Population Target",
                      "Disposal Rate without Transformation (pounds/person/day): Population Annual","Transformation Rate (pounds/person/day): Population Annual",
                      "Calculated Disposal Rate (pounds/person/day) Population Annual","Disposal Rate without Transformation (pounds/person/day): Employment Target",
                      "Transformation Rate (pounds/person/day): Employment Target","Calculated Disposal Rate (pounds/person/day) Employment Target",
                      "Disposal Rate without Transformation (pounds/person/day): Employment Annual","Transformation Rate (pounds/person/day): Employment Annual",
                      "Calculated Disposal Rate (pounds/person/day) Employment Annual"
)

write.csv(df2008,file="Dropbox/NewMet Project/Diversion2008.csv")







df <- c()
for (i in 1:700){
  
  df<- cbind(df,urlFun(year=2009,jurID = i))
  
}
df2009 <- t(df)
colnames(df2009) <- c("Jurisdiction","County","Year","Reporting-Year-Disposal-Amount","Disposal Reduction Credits","Disaster Waste",
                      "Medical Waste","Regional Diversion Facility Residual Waste","C&D Waste","Out-of-State Export","Class II Waste",
                      "Other Disposal Amount","Total Disposal Reduction Credit Amount","Total Adjusted Reporting-Year Disposal Amount",
                      "Reporting-Year Transformation Waste","Reporting-Year Population",
                      "Reporting-Year Employment","Disposal Rate without Transformation (pounds/person/day): Population Target",
                      "Transformation Rate (pounds/person/day): Population Target","Calculated Disposal Rate (pounds/person/day) Population Target",
                      "Disposal Rate without Transformation (pounds/person/day): Population Annual","Transformation Rate (pounds/person/day): Population Annual",
                      "Calculated Disposal Rate (pounds/person/day) Population Annual","Disposal Rate without Transformation (pounds/person/day): Employment Target",
                      "Transformation Rate (pounds/person/day): Employment Target","Calculated Disposal Rate (pounds/person/day) Employment Target",
                      "Disposal Rate without Transformation (pounds/person/day): Employment Annual","Transformation Rate (pounds/person/day): Employment Annual",
                      "Calculated Disposal Rate (pounds/person/day) Employment Annual"
)

write.csv(df2009,file="Dropbox/NewMet Project/Diversion2009.csv")



df <- c()
for (i in 1:700){
  
  df<- cbind(df,urlFun(year=2010,jurID = i))
  
}
df2010 <- t(df)
colnames(df2010) <- c("Jurisdiction","County","Year","Reporting-Year-Disposal-Amount","Disposal Reduction Credits","Disaster Waste",
                      "Medical Waste","Regional Diversion Facility Residual Waste","C&D Waste","Out-of-State Export","Class II Waste",
                      "Other Disposal Amount","Total Disposal Reduction Credit Amount","Total Adjusted Reporting-Year Disposal Amount",
                      "Reporting-Year Transformation Waste","Reporting-Year Population",
                      "Reporting-Year Employment","Disposal Rate without Transformation (pounds/person/day): Population Target",
                      "Transformation Rate (pounds/person/day): Population Target","Calculated Disposal Rate (pounds/person/day) Population Target",
                      "Disposal Rate without Transformation (pounds/person/day): Population Annual","Transformation Rate (pounds/person/day): Population Annual",
                      "Calculated Disposal Rate (pounds/person/day) Population Annual","Disposal Rate without Transformation (pounds/person/day): Employment Target",
                      "Transformation Rate (pounds/person/day): Employment Target","Calculated Disposal Rate (pounds/person/day) Employment Target",
                      "Disposal Rate without Transformation (pounds/person/day): Employment Annual","Transformation Rate (pounds/person/day): Employment Annual",
                      "Calculated Disposal Rate (pounds/person/day) Employment Annual"
)

write.csv(df2010,file="Dropbox/NewMet Project/Diversion2010.csv")



df <- c()
for (i in 1:700){
  
  df<- cbind(df,urlFun(year=2011,jurID = i))
  
}
df2011 <- t(df)
colnames(df2011) <- c("Jurisdiction","County","Year","Reporting-Year-Disposal-Amount","Disposal Reduction Credits","Disaster Waste",
                      "Medical Waste","Regional Diversion Facility Residual Waste","C&D Waste","Out-of-State Export","Class II Waste",
                      "Other Disposal Amount","Total Disposal Reduction Credit Amount","Total Adjusted Reporting-Year Disposal Amount",
                      "Reporting-Year Transformation Waste","Reporting-Year Population",
                      "Reporting-Year Employment","Disposal Rate without Transformation (pounds/person/day): Population Target",
                      "Transformation Rate (pounds/person/day): Population Target","Calculated Disposal Rate (pounds/person/day) Population Target",
                      "Disposal Rate without Transformation (pounds/person/day): Population Annual","Transformation Rate (pounds/person/day): Population Annual",
                      "Calculated Disposal Rate (pounds/person/day) Population Annual","Disposal Rate without Transformation (pounds/person/day): Employment Target",
                      "Transformation Rate (pounds/person/day): Employment Target","Calculated Disposal Rate (pounds/person/day) Employment Target",
                      "Disposal Rate without Transformation (pounds/person/day): Employment Annual","Transformation Rate (pounds/person/day): Employment Annual",
                      "Calculated Disposal Rate (pounds/person/day) Employment Annual"
)

write.csv(df2011,file="Dropbox/NewMet Project/Diversion2011.csv")


df <- c()
for (i in 1:700){
  
  df<- cbind(df,urlFun(year=2012,jurID = i))
  
}

df2012 <- t(df)
colnames(df2012) <- c("Jurisdiction","County","Year","Reporting-Year-Disposal-Amount","Disposal Reduction Credits","Disaster Waste",
                      "Medical Waste","Regional Diversion Facility Residual Waste","C&D Waste","Out-of-State Export","Class II Waste",
                      "Other Disposal Amount","Total Disposal Reduction Credit Amount","Total Adjusted Reporting-Year Disposal Amount",
                      "Reporting-Year Transformation Waste","Reporting-Year Population",
                      "Reporting-Year Employment","Disposal Rate without Transformation (pounds/person/day): Population Target",
                      "Transformation Rate (pounds/person/day): Population Target","Calculated Disposal Rate (pounds/person/day) Population Target",
                      "Disposal Rate without Transformation (pounds/person/day): Population Annual","Transformation Rate (pounds/person/day): Population Annual",
                      "Calculated Disposal Rate (pounds/person/day) Population Annual","Disposal Rate without Transformation (pounds/person/day): Employment Target",
                      "Transformation Rate (pounds/person/day): Employment Target","Calculated Disposal Rate (pounds/person/day) Employment Target",
                      "Disposal Rate without Transformation (pounds/person/day): Employment Annual","Transformation Rate (pounds/person/day): Employment Annual",
                      "Calculated Disposal Rate (pounds/person/day) Employment Annual"
)

write.csv(df2012,file="Dropbox/NewMet Project/Diversion2012.csv")


df <- c()
for (i in 1:700){
  
  df<- cbind(df,urlFun(year=2013,jurID = i))
  
}

df2013 <- t(df)
colnames(df2013) <- c("Jurisdiction","County","Year","Reporting-Year-Disposal-Amount","Disposal Reduction Credits","Disaster Waste",
                      "Medical Waste","Regional Diversion Facility Residual Waste","C&D Waste","Out-of-State Export","Class II Waste",
                      "Other Disposal Amount","Total Disposal Reduction Credit Amount","Total Adjusted Reporting-Year Disposal Amount",
                      "Reporting-Year Transformation Waste","Reporting-Year Population",
                      "Reporting-Year Employment","Disposal Rate without Transformation (pounds/person/day): Population Target",
                      "Transformation Rate (pounds/person/day): Population Target","Calculated Disposal Rate (pounds/person/day) Population Target",
                      "Disposal Rate without Transformation (pounds/person/day): Population Annual","Transformation Rate (pounds/person/day): Population Annual",
                      "Calculated Disposal Rate (pounds/person/day) Population Annual","Disposal Rate without Transformation (pounds/person/day): Employment Target",
                      "Transformation Rate (pounds/person/day): Employment Target","Calculated Disposal Rate (pounds/person/day) Employment Target",
                      "Disposal Rate without Transformation (pounds/person/day): Employment Annual","Transformation Rate (pounds/person/day): Employment Annual",
                      "Calculated Disposal Rate (pounds/person/day) Employment Annual"
)

write.csv(df2013,file="Dropbox/NewMet Project/Diversion2013.csv")