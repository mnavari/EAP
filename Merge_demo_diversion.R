## Mahdi Navari  Jully  22 2015

setwd("C:\\mahdi\\classes\\NewMetData\\data")
df11  <- read.csv ("ACS_13_5YR_DP05_with_ann.csv", sep=',' ,  header= TRUE )
#df22 <- read.csv ("Disposal2007.csv", sep=',' ,  header= TRUE )
disposal <- read.csv ("divmerged2013.csv", sep=',' ,  header= TRUE )

# clean the  data
demo <- subset(df11, !grepl("CDP", df11$Geography))



#demo <- df111[1:10,3:5]
#disposal <- df22[1:10,1:4]
demo$Geography <- as.character(demo$Geography)
disposal$Jurisdiction <- as.character(disposal$Jurisdiction)

index <- sapply(disposal$Jurisdiction, function(x) {
    agrep(x, demo$Geography, ignore.case=TRUE, max.distance = 1)
}
)
index <- unlist(index)
demo$Geography[index] <- names(index)
merged <- merge(disposal, demo, by.x = "Jurisdiction", by.y = "Geography")


