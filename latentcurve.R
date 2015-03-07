# print(114+15+17+ 8) 
# print(82 + 18 + 14 + 22)
setwd("/Users/arindambose/Dropbox/growth-curve-analysis")
print(filelist <- list.files())
require(foreign)
mydata <- read.spss(filelist[2], to.data.frame = T)
print(length(mydata))
print(summary(mydata[2:20]))

write.csv(mydata, file = "mydata.csv", 
          row.names = FALSE)