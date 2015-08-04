setwd("//Users/SUMA/Documents/Learn-IT/Newmetdata/Data/Final/Excel")

demo <- read.csv("Diversion-Political-Social2010t013.csv", header = TRUE)

#unique(demo$Geography)
#str(demo)


## Calucation for diversion rate
diversion <- function(pptarget, ppactual){
  
pptarget <-  as.numeric(levels(pptarget))[pptarget]
ppactual <-  as.numeric(levels(ppactual))[ppactual]
gentarget <- pptarget/.5
divrate <- (gentarget - ppactual) / gentarget
return(divrate)
}

demo$DiversionRate <- diversion(demo$Calculated.Disposal.Rate..pounds.person.day..Population.Target,
          demo$Calculated.Disposal.Rate..pounds.person.day..Population.Annual)

write.csv(demo, "Diversion-Political-Social2010t013-Diversionrate.csv")


##E6 <- 7.1/.5
##(E6-5.2)/E6

#Diversion Rate= apply(demo, 1, 
#                      diversion(demo$Calculated.Disposal.Rate..pounds.person.day..Population.Target,
#                                demo$Disposal.Rate.without.Transformation..pounds.person.day...Population.Annual)

#transform(demo, Diversion Rate= apply(demo, 1, 
#    diversion(demo$Calculated.Disposal.Rate..pounds.person.day..Population.Target,
#              demo$Disposal.Rate.without.Transformation..pounds.person.day...Population.Annual)))

