setwd("//Users/SUMA/Documents/Learn-IT/Newmetdata/Data/Final/")

table <- read.csv("Housing.csv", header = TRUE)

columns <- subset(names(table),!grepl('Margin.of.Error',names(table)))

table2 <- table[,columns]
summary(table2)


write.csv(table2, "HousingNew.csv")
