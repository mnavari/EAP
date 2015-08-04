## Mahmoud Kamalzare (July 23 2015)

setwd("C:/Users/Mahmoud/Desktop/NewMetData/Test")

# Census data ...........................
census <- read.csv ("Demographics.csv", skip=1, sep=',', header= TRUE )
#census <- read.csv ("Economics.csv", skip=1, sep=',', header= TRUE )
#census <- read.csv ("Housing.csv", skip=1, sep=',', header= TRUE )
#census <- read.csv ("Social.csv", skip=1, sep=',', header= TRUE )
#str(census)

# Diversion data ...........................
disposal <- read.csv ("Diversion07_13.csv", sep=',', header= TRUE )
#str(disposal)

# Clean the data ...........................
census$Geography <- gsub(" ", "", census$Geography, fixed = TRUE)
disposal$Jurisdiction <- gsub(" ", "", disposal$Jurisdiction, fixed = TRUE)

# Merge the two tables .....................
merged <- merge(census, disposal, by.x = c("Geography","year"), by.y = c("Jurisdiction","Year"))

# Save the merged table ....................
write.csv(merged, file="Demographics_Diversion_2010_to_2013.csv")
#write.csv(merged, file="Economics_Diversion_2010_to_2013.csv")
#write.csv(merged, file="Housing_Diversion_2010_to_2013.csv")
#write.csv(merged, file="Social_Diversion_2011_to_2013.csv")
