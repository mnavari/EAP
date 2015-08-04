setwd("//Users/SUMA/Documents/Learn-IT/Newmetdata/Data/Final/Excel/")
getwd()
library(dplyr)
library(caret)
library(ggplot2)
library(cluster)
library(fpc)
demo <- read.csv("Demographics-Final.csv", header = TRUE)
names(demo)
##demo[is.na(demo$DiversionRate),]
demo2 <- subset(demo, !is.na(demo$DiversionRate))
demo3 <- demo2[demo2$year == 2010,c(12:23)]
summary(demo2)
demo2$year
fit <- kmeans(demo3,3, iter.max = 100)

clusplot(demo3,fit$cluster, color = TRUE, shade = TRUE, labels = 2, line = 0)
plot(demo3, col = fit$cluster)

