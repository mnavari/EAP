setwd("//Users/SUMA/Documents/Learn-IT/Newmetdata/Data/Final/Excel/")

library(dplyr)

# Read the data of Diversion-Census-Political of demographics,economics,social&housing ####

demo <- read.csv("Diversion-Political-Housing2010t013-Diversionrate.csv", 
                 header = TRUE, stringsAsFactors = FALSE)

# Remove Percent features ####
columns <- subset(names(demo),!grepl('Estimate',names(demo)) | grepl('Median', names(demo)) | grepl('Mean', names(demo)))

demograph <- demo[,columns]

# Remove columns having all NAs ####
nacolumns <- demograph[,colSums(is.na(demograph)) == nrow(demograph)]
nalist <- names(nacolumns)
demograph <- demograph[,!(names(demograph) %in% nalist)]

# Delete fields "Disposal.Rate.without.Transformation..pounds.person.day...Population.Target"
# Delete fields "Disposal.Rate.without.Transformation..pounds.person.day...Employment.Target"
# sapply(demograph,mode)

# convert to numeric values to numeric types ####
numdemo <- demograph[,grepl("[0-9]",demograph[,0:ncol(demograph)])]
numericfld <- names(numdemo)
numbonly <- as.data.frame(lapply(numdemo[,numericfld], function(x){as.numeric(gsub(",", "", x))}))

numbonly$DiversionRate
demographics <- demograph[, !names(demograph) %in% numericfld]
demographics <- merge(demographics, numbonly, by = "row.names", all.x = TRUE)
demographics <- demographics[order(demographics$X),]

# Remove columns having 90% zeros ####
##finaldemo <- demographics[, colSums(demographics != "(X)", na.rm = TRUE) == nrow(demographics) ]
finaldemo <- demographics[, colSums(demographics == 0, na.rm = TRUE) > (nrow(demographics)*0.9) ]
zerofld <- names(finaldemo)
finaldemo <- demographics[,!names(demographics) %in% zerofld]

#classify diversion rate
breaks <- c(-0.2,0.50,0.55,0.60,0.65,0.70,0.75,0.80,1)
labels <- c("Low", "50-55", "55-60","60-65", "65-70","70-75","75-80","High" )
Divrateclasses <- cut(finaldemo$DiversionRate, breaks, labels)
finaldemo <- cbind(finaldemo,Divrateclasses)

finaldemo$Annual.Report.Review.Status <- NULL
finaldemo$Jurisdiction.Review.Status <- NULL
finaldemo$Compliance.Status <- NULL

write.csv(finaldemo, "Housing-Final.csv")
