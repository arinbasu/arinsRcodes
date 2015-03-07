setwd("/Users/arindambose/Documents/kathleenliberty/dataanalysis")
filelist <- list.files()
print(list.files())
print(filelist)
# read the datasets
# Write a function for univariate tables
unitable <- function(x){
  x1 <- table(x)
  x2 <- round(prop.table(x1)*100, 2)
  x3 <- data.frame(x1, x2)
  return(data.frame(x3[,1], x3[,2], x3[,4]))
}

# function for bivariate tables

bitable <- function(x,y){
  xytable <- table(x,y)
  xyrow <- round(prop.table(xytable, 1)*100, 2)
  pvalue <- chisq.test(xytable)
  xytable2 <- data.frame(xytable, xyrow)
  
}

require(foreign)
prestudydata <- read.spss(filelist[8], 
                          to.data.frame = TRUE,
                          use.value.labels = TRUE)

teachreportdata <- read.spss(filelist[1], 
                             to.data.frame = TRUE,
                             use.value.labels = TRUE)


# put all outputs to output.txt
sink("output.txt")
sumprestudydf <- summary(prestudydata)
names(prestudydata)
sumprestudydf
sink()
#print(sumprestudydf)
print(table(prestudydata$bpitotal))
print(table(prestudydata$school))
write.csv(prestudydata, file = "predata.csv", sep = ",", col.names = TRUE)

print(table(prestudydata$mostrt))
prestudydata$mostrt1 <- as.factor(prestudydata$mostrt)
levels(prestudydata$mostrt1) <- c("February", "March", "April",
                                  "May", "June", "July",
                                  "August", "Sept", "Oct", "Nov", "Dec")
print(table(prestudydata$mostrt1))
monthstarted <- unitable(prestudydata$mostrt1)
names(monthstarted) <- c("Month", "Number", "Percent")
# Graph the number of students that would be admitted before earthquake
# las = 2 for names to be perpendicular to the x axis
barplot(monthstarted$Percent, names.arg = monthstarted$Month,
        las = 2,
        xlab = "Month Started",
        ylab = "Percent of Students",
        main = "Percentage of Students Admitted in Various Months")
