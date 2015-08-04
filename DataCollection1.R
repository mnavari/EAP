library(XML) # Extract XML

library(reshape2) # 

getUrls <- function(y1,y2){
  root='http://www.calrecycle.ca.gov/LGCentral/Reports/DiversionProgram/JurisdictionDiversionDetail.aspx?JurisdictionID='
  urls <- NULL
  for (year in y1:y2){
    for (jur_id in 1:2){
      
      urls <- c(urls,(paste(root, jur_id , '&Year=' , year , sep="")))
  
    }
  }
  return(urls)
}
# call function to generate urls
urls <- getUrls(y1=2007,y2=2007)

#get data for each of them and store that data

PMI <- data.frame()
#results=NULL
for (url in urls){
  ##rawPMI <- readHTMLTable('http://www.calrecycle.ca.gov/LGCentral/Reports/DiversionProgram/JurisdictionDiversionDetail.aspx?JurisdictionID=1&Year=2007')
  rawPMI <- readHTMLTable(url)
  LoadPMI <- data.frame(rawPMI[[1]], rawPMI[[3]])
  PMI <- rbind(PMI,LoadPMI)
  #PMI <- rbind(data.frame(rawPMI[[1]]))
  #PMI <- cbind(PMI$V2, rawPMI[[3]])
}
str(LoadPMI)
#rawPMI
#names(PMI)[1] <- 'Year'