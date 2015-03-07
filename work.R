getwd()
require(foreign)
require(gdata)
setwd("C:/Users/Arin-Mou-Maurine/Dropbox/data")
filelist <- list.files()
mydata <- read.csv("mydata.csv", na.strings = ".")
sink("variables.txt")
names(mydata)
sink()
#mydata <- read.xls("dataset.xls")
#summary.1 <- summary(mydata)
## write functions in this space  
tablefnc <- function(x,y = mydata$medianyld){
  tbl1 <- table(x,y)
  tbl1pc <- round(prop.table(tbl1, 1)*100, 3)
  tbl1p <- prop.trend.test(tbl1[,1], (tbl1[,1]+tbl1[,2]), 
                           score = levels(x))$p.value
  
  return(cbind(tbl1, tbl1pc, tbl1p))   
}


# Get the means of average yield 2 by gender, pdpcq
yldpdpcq <- tapply(mydata[, 38], mydata[,26], mean, na.rm = T)
yldpdpcq1 <- rbind(yld2pdpcq, yldpdpcq )
yldpdpcq2 <- tablefnc(mydata$pdpcq, mydata$medianyld)

