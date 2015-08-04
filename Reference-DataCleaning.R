
library(dplyr)

# Read the data of Diversion-Census-Political of demographics,economics,social&housing

setAs("character", "num.with.commas", 
      function(from) as.numeric(gsub(",", "", from) ) )

demo <- read.csv("Diversion-Political-Demographiccensus-Diversionrate.csv", 
                 header = TRUE, stringsAsFactors = FALSE)
warnings()

demographics <- demograph
sapply(demographics,mode)
potential_numcol <- as.character()

for(char in seq_along(names(demographics))){
  potential_numcol[char] <- all(!grepl("[a-zA-Z]",demographics[,char]))
}
# and now just convert only the numeric ones
i
potential_numcol

all(!grepl("[a-zA-Z]",demographics[,))

demographics <- sapply(demographics[,potential_numcol[i]],as.numeric(gsub(",","", data)))

zerocolumns <- demograph[,apply(demograph,2,function(x) all(demograph==0))]

allcoll <- lapply(demograph,class)

rownum <- 
  
  demographics<-demograph[,colSums(demograph != '0') > rownum]

demographics <- demograph %>% mutate_each(funs(type.convert(.)))

col2cvt <- 8:108
tmp[,col2cvt] <- lapply(tmp[,col2cvt],function(x){as.numeric(gsub(",", "", x))})
str(demographics[, ])