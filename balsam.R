setwd("/Users/arindambose/Documents/PhD supervision/balsam")
print(list.files())
require(foreign)
thisdata <- read.spss("Part Two - Data Summary.sav", to.data.frame = TRUE)
shortdata <- read.spss("2 Part Two - R.sav", to.data.frame = TRUE)
print(names(thisdata))
write.dta(thisdata, "thisdata.dta")
write.dta(shortdata, "shortdata.dta")

print(names(shortdata))

plot(shortdata$CIDS, shortdata$HbAc)

print(names(shortdata))

