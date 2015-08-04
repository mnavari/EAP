setwd("//Users/SUMA/Documents/Learn-IT/Newmetdata/Data/Final/Excel/")
getwd()
library(dplyr)
library(caret)
library(ggplot2)

demo <- read.csv("Demographics-Final.csv", header = TRUE)
str(demo)
names(demo)
ggplot(demo, aes(x= demo$DiversionRate)) + geom_histogram()

demo <- as.data.frame(lapply(demo, function(x) {ifelse(x == "NULL", NA, x)}))
demo$DiversionRate <- demo$DiversionRate * 100
CorrelationMatrix <- cor(demo[,c(10:55,81)],use="pairwise.complete.obs", method="kendall")
highlyCorrelated <- findCorrelation(CorrelationMatrix, cutoff = 0.75)
highlyCorrelated
highcor <- as.data.frame(CorrelationMatrix[,c(19,20,18,46,27,42,31,21,44,29,30,39,1,23,25)])

control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Divrateclasses ~., data=demo, method="lvq", preProcess="scale", trControl=control)
warnings()
str(demo)
names(demo)
plot(demo[,1:10])

