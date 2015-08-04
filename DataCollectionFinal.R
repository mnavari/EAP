library(XML) # Extract XML

library(reshape2) # 

getUrls <- function(y1,y2){
  root='http://www.calrecycle.ca.gov/LGCentral/Reports/DiversionProgram/JurisdictionDiversionDetail.aspx?JurisdictionID='
  urls <- NULL
  for (year in y1:y2){
    for (jur_id in 1:700){
      
      urls <- c(urls,(paste(root, jur_id , '&Year=' , year , sep="")))
    }
  }
  return(urls)
}
# call function to generate urls
PMI <- data.frame()
urls <- getUrls(y1=2011,y2=2014)


#get data for each of them and store that data

#results=NULL
for (url in urls){
  ##rawPMI <- readHTMLTable('http://www.calrecycle.ca.gov/LGCentral/Reports/DiversionProgram/JurisdictionDiversionDetail.aspx?JurisdictionID=1&Year=2007')
  rawPMI <- readHTMLTable(url, header = FALSE, )
  line1 <- rawPMI[[1]]
  LoadPMI <- data.frame(line1$V2[[1]], line1$V2[[2]], line1$V4[[1]])
  line2 <- rawPMI[[3]]
  LoadPMI <- cbind(LoadPMI,line2)
  line3 <- rawPMI[[2]]
  LoadPMI <- cbind(LoadPMI,line3$V2[[1]],line3$V2[[9]],line3$V2[[11]],line3$V2[[12]],line3$V2[[13]],line3$V2[[14]], line3$V2[[15]])
  PMI <- rbind(PMI,LoadPMI[3:5,])
}

colnames(PMI) <- c("Jurisdiction", "County", "Year", "Description", "Population Target", "Population Annual", 
                   "Emp Target", "Emp Annual", "Reported Disposal Amount(tons)", "Other Disposal Amount(tons)",
                   "Total Disposal Reduction Credit Amount (tons)", "Total Adjusted Reporting-Year Disposal Amount (tons)",
                   "Reporting-Year Transformation Waste (tons)","Reporting-Year Population",
                   "Reporting-Year Employment")

FinalPMI <- PMI[as.character(PMI$County) != "",]


write.csv(FinalPMI , file = '//Users/SUMA/Documents/Learn-IT//Newmetdata/Data/Disposal2011.csv' )