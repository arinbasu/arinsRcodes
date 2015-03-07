# require(foreign)
# locate and read vinod's datasets
# set the working directory to where vinod's data are kept
# setwd("/Users/arindambose/Documents/vinod")
# print(list.files())

# thedata <- read.spss("vinoddata.sav", to.data.frame = T)

# write a function to generate cross tables with percentages
# have only row percentages and two places to the decimal
# have raw data and columns next to each other

crstbls <- function(x,y){
  txy <- table(x,y)
  pctxy <- round(prop.table(txy, 1)*100, 2)
  fin.txy <- cbind(txy,pctxy)
  return(fin.txy)
}

# Generate a cross tabulation between pre and post intervention for
# status * team

st.tm2 <- table(thedata$Team, thedata$status)
pct.tm <- round(prop.table(st.tm2, 1)*100,2)
raw.pct <- cbind(st.tm, pct.tm)

# Cross tabulate teams and reasons of review

cr.team.res <- crstbls(thedata$Team, thedata$Reason)



#sink("vinodoutputs.txt")
#print(names(thedata))
print(summary(thedata))
print(raw.pct)
print(cr.team.res)
#sink()