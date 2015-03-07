setwd("/Users/arindambose/Google Drive")
library(foreign)
data <- data.frame(read.dta("testout.dta"))
attach(data)
x2 <- x*2 
data2 <- cbind(data, x2)
write.dta(data2, "testing.dta")