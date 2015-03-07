mrt <- c(11,9,2)
mrtpct <- round(mrt/sum(mrt)*100,2)
# print(mrtpct)

## percentage function
# Take a list as a parameter and generate percentages

percentfunc <- function(x){
  xpct <- round(x/sum(x)*100,2)
  return(xpct)
}

mrtprof <- c(1,11,2,1,1,1,1)
otserv <- c(24,2,1,4,1,1)
# print(percentfunc(otserv))
otprof <- c(6,8,8,5,1,1)
# print(percentfunc(otprof))
physerv <- c(7,2,1)
physup <- c(2,2,1,1,1,1)

# print(percentfunc(physerv)); print(percentfunc(physup))

sltserv <- c(1,1,1,1,1); sltsup <- c(2,1)
# print(percentfunc(sltserv)); print(percentfunc(sltsup))

othserv <- c(3,2,1); othsup <- c(2,1)
# print(percentfunc(othserv)); print(percentfunc(othsup))

justify <- c(58,103,29,2,7)
newknw <- c(76,105,13,4,1)
scary <- c(5,21,94,73,6)
confusing <- c(10,18,114,53,4)
understanding <- c(40,118,32,2,7)
nonunderst <- c(12,43,93,48,3)
onyrelsup <- c(9,27,98,58,7)
broadens <- c(66,110,13,5,5)
alongprof <- c(36,55,80,18,10)

print(percentfunc(justify)); 
print(percentfunc(newknw)); 
print(percentfunc(scary));
print(percentfunc(confusing));
print(percentfunc(understanding));
print(percentfunc(nonunderst));
print(percentfunc(onyrelsup));
print(percentfunc(broadens));
print(percentfunc(alongprof));
















