dbcol1 <- c("ercrit", "miyes", "mino")
dbcol2 <- c("pos", 50, 91)
dbcol3 <- c("neg", 5, 211)     

db1 <- data.frame(rbind(dbcol1, dbcol2, dbcol3))


testpos <- c(50,91)
testneg <- c(5,211)
mydata <- data.frame(rbind(testpos, testneg))
names(mydata) = c("MIyes", "MIno")
mydata$coltotal <- mydata$MIyes + mydata$MIno

attach(mydata)

sens <- (MIyes[1]/sum(MIyes))*100
spec <- (MIno[2]/sum(MIno))*100
ppv <- (MIyes[1]/coltotal[1])*100
npv <- (MIno[2]/coltotal[2])*100

allMI <- sum(MIyes)
totaltotal <- sum(coltotal)
rateMIgPain <- allMI/totaltotal * 100


# create a case control study
# here, we are setting up a case control study to find out the ppv and npv

cases <- 100
controls <- 100

# screened positive among non-MI

poscontrols <- round((testpos[2]/sum(MIno))*controls,0)
negcontrols <- controls - poscontrols
poscases <- round(MIyes[1]/sum(MIyes)*cases,0)
negcases <- cases - poscases

positives <- c(poscases, poscontrols)
negatives <- c(negcases, negcontrols)
total <- c(cases, controls)
newdata <- data.frame(rbind(positives, negatives, total))
newdata$total <- newdata$X1 + newdata$X2
names(newdata) = c("cases", "controls", "total")
# attach(newdata)
ppvnew <- newdata$cases[1]/newdata$total[1]*100
npvnew <- newdata$controls[2]/newdata$total[2]*100
sensnew <- newdata$cases[1]/newdata$cases[3]
specnew <- newdata$controls[2]/newdata$controls[3]

# influence of sample size on this can be shown as follows

thisOutput200 <- cbind(cases, controls, ppvnew, npvnew, sensnew, specnew)
thisOutput55 <- cbind(cases, controls, ppvnew, npvnew, sensnew, specnew)
thisOutput100 <- cbind(cases, controls, ppvnew, npvnew, sensnew, specnew)

varoutputs <- data.frame(rbind(thisOutput55, thisOutput100, thisOutput200))

# sensnew and specnew can still be calculated from the case control study
# here we see how we calculate the ppv and npv from a case control study
# prevalence will be the prevalence of the disease in the entire population,
# irrespective of what the test results

totalMIcases <- sum(MIyes)
grandtotal <- sum(MIyes) + sum(MIno)
prev1 <- totalMIcases/grandtotal

# we are going to demonstrate that 
# for accurate estimation of PPV and NPV 
# which are the real things about screening studies, you require
# either (1), a study that accurately measures the prevalence of the disease condition,
# or (2), an estimation of the prevalence of the disease in question
# let's set prev to be equal to some arbitrary value prev1

# when using study, we say prev = prev1, or else, set a value to prev

prev <- prev1
# prev <- 0.1


arbitcount <- 10000
# We can write a function that can either take on the estimates from the case
# control study, or any study, or some arbitrary value to estimate the 
# ppv and npv from any value

calcpv <- function(arbitcount = 10000, prev, sens, spec){
  # those in a sample population of N (here, arbitcount) with MI
arbitmi <- arbitcount * prev
# those in a sample populatoin of N with no MI
arbitnotwithmi <- arbitcount - arbitmi

# Number of true positive, will be sensitivity and arbitmi

trueposcount <- arbitmi * sens
truenegcount <- arbitnotwithmi * spec
fncount <- arbitmi - trueposcount
fpcount <- arbitnotwithmi - truenegcount

recpositives <- trueposcount + fpcount
recnegatives <- fncount + truenegcount

ppvrec <- round(trueposcount/recpositives, 5)
npvrec <- round(truenegcount/recnegatives, 5)
  
  return(cbind(ppvrec, npvrec))
}

# Let's run a test with sensitivity and specificity both at 97%
# but we vary the prevalence of the disease and set arbitcount at 100000

pv10pct <- calcpv(100000, 0.1, 0.97, 0.97)
pv1pct <- calcpv(100000, 0.01, 0.97, 0.97)
pv01pct <- calcpv(100000, 0.001, 0.97, 0.97)
pv001pct <- calcpv(100000, 0.0001, 0.97, 0.97)

# If we create a table to see how it all stack up, see

TenPercent <- c(0.10, pv10pct)
Onepct <- c(0.01, pv1pct)
Onethou <- c(0.001, pv01pct)
morepct <- c(0.0001, pv001pct)

tabletwo <- data.frame(rbind(TenPercent, Onepct, Onethou, morepct))
names(tabletwo) <- c("Prevalence", "PPV", "NPV")


# export this table to csv and work on it
write.table(tabletwo, file="prevppv.csv")
# As prevalence falls off, the PPV becomes too low to be of any meaningful use

# Next we focus on what happens if screening does or does not take place

hypertensive <- c(90, 810)
normotensive <- c(455, 8645)
total <- c(545, 9455)

tablenoscreen <- data.frame(rbind(c(hypertensive, sum(hypertensive)), 
                       c(normotensive, sum(normotensive)),
                       c(total, sum(total))))

names(tablenoscreen) = c("Death", "No Death", "Total")


# Creat the table for screened groups here the survival is 70%

survival <- 0.70

hypertscr <- c(90*survival, 810-(90*survival))
normotenscr <- normotensive

tablescreened <- data.frame(rbind(hypertscr, normotenscr))
names(tablescreened) = c("Death", "No Death")

diffdeath <- tablenoscreen$Death[1] - tablescreened$Death[1]

# total deaths in no screen

totdeathnoscreen <- tablenoscreen$Death[3]
totdeathscreen <- sum(tablescreened$Death)

# % difference in death in the absence of screening

pctdiff <- (totdeathnoscreen - totdeathscreen)/totdeathnoscreen * 100

# Add three more pieces of information\

# proportion of people tested positive for a screening or diagnostic test prpscrn

prpscrn <- 0.09

# Outcome of people who test positive or not for the screening programme

deathcond1 <- 0.10
deathcond2 <- 0.05

# effectiveness of treatment

effectrx <- 0.70


                            
# detach(newdata)

detach(mydata)

totalpositivesarticle <- 2874+2448+1118+883+368+972
pyannscreening <- 235584
posannrate <- pyannscreening/totalpositivesarticle
