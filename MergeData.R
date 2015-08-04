library(DataCombine)
library(stringr)
setwd("//Users/SUMA/Downloads/ACS_13_5YR_DP05")
Demotable <- read.csv('Sheet 1-Table 1.csv', sep = ',', header = TRUE)

head(Demotable[grep("CDP", Demotable$Geography),])

Democensus <- Demotable
head(subset(Democensus, !grep("CDP", Democensus$Geography))
            
Democensus <- subset(Democensus, !grepl("CDP", Democensus$Geography))


disp2007 <- read.csv('//Users/SUMA/Documents/Learn-IT/Newmetdata/Data/Disposal2011.csv', header = TRUE)
dim(disp2007$Year == '2011')

disp2011 <- disp2007[disp2007$Year == '2011',]
disp2013 <- disp2007[disp2007$Year == '2013',]

#test <- gregexpr(pattern ='city, California','Bakersfield city, California')
#test1 <- str_locate_all(pattern ='city, California','Bakersfield city, California')


write.csv(Democensus, file = "//Users/SUMA/Documents/Learn-IT/Newmetdata/Data/ACS_13_5YR_City.csv")
Replaces <- data.frame(from = "city, California", to = "")
Demofinal <- FindReplace(data = Democensus, Var = "Geography", replaceData = Replaces,
            from = "from", to = "to", exact = FALSE)

write.csv(Demofinal, file = "//Users/SUMA/Documents/Learn-IT/Newmetdata/Data/ACS_13_5YR_CityOnly.csv" )
