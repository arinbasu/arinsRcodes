
setwd("C:/Users/arinbasu/Documents/Projects/ArsenicProjectwithAllan/workon090909_eod")
list.files()
newdata <- read.csv("workdata.csv")
summary(newdata)
plot(newdata$bmi1)
names(newdata)
