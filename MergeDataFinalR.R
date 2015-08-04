setwd("//Users/SUMA/Documents/Learn-IT/Newmetdata/Data/Final/")

house  <- read.csv ("Demographics.csv", sep=',' ,  header= TRUE )
disposal <- read.csv ("Diversion07_13.csv", sep=',' ,  header= TRUE )

columns <- subset(names(house),!grepl('Margin.of.Error',names(house)))

demo <- house[,columns]

demo$Geography <- as.character(demo$Geography)
disposal$Jurisdiction <- as.character(disposal$Jurisdiction)


trim.trailing <- function (x) sub("\\s+$", "", x)
for(i in 1:length(demo$Geography)){
  demo$Geography[i]<-trim.trailing(demo$Geography[i])
}

merged <- merge(disposal, demo, by.x = c("Jurisdiction", "Year") , by.y = c("Geography", "X"))
write.csv(merged, "Diverson-Demographics20102013.csv")
