setwd("/Users/arindambose/Documents/anns-data/ann_new")
### the following were old data analysed ####
# mydata <- read.csv("casmosnhi.csv", header = T)
#converted <- subset(mydata, mydata$status == "NP")
# mydata$brca <- (mydata$site10 == "C501" | mydata$site10 == "C502" | mydata$site10 == "C503" | 
#  mydata$site10 == "C504" | mydata$site10 == "C505" | mydata$site10 == "C505" |
#  mydata$site10 == "C506" | mydata$site10 == "C508" | mydata$site10 == "C509")
# tbrca <- table(mydata$brca)

# brca <- subset(mydata, mydata$brca == "TRUE")
# names(brca)
#######
varnames <- names(mydata)

# list of functions
# 1. Single Variable table (n,%)
# Ordered in decreasing percentages
tbl1 <- function(x){
  t1 <- table(x)
  t1p <- round(prop.table(t1)*100, 2)
  t1c <- data.frame(cbind(t1, t1p))
  return(t1c[ order(-t1),])
}
#### again outputs of old analyses ###
# tabulate how many values did not match
# mmtbl <- table(mydata$mismatch)
# list of mstrnhi and eventnhi for those with mismatch
# master1 <- mydata$mstrnhi[mydata$mismatch == "mismatch"]
# event1 <- mydata$eventnhi[mydata$mismatch == "mismatch"]

# tables, single variable

# tsite10 <- tbl1(mydata$site10)
# tmorph <- tbl1(mydata$morph)
# tdxbasis <- tbl1(mydata$dxbasis)
# tethnic <- tbl1(mydata$ethnicity)
# toccup <- tbl1(mydata$occup)
# tprogstat <- tbl1(mydata$progstat)
# tHer2Stat <- 
#tabl2 <- table(mydata$site10, mydata$morph)
#print(tabl2)
#print(names(mydata))
# print(tmorph)
# print(tsite10)
  
# print(mmtbl)

# print(master1)
# print(event1)
#########

## In the following, we shall start a new phase of data analysis with the new dataset
## dataset = casmosnhi
mydata1 <- read.csv("cmnhmrgd.csv", header = TRUE)
## split the data frame by site 10
site10 <- split(mydata1, mydata1$site10)
