## work on 26-03-2010
## need to clean up the code page for faster finding of stuff and correction
# here is the working directory, set in:
setwd("/home/arinbasu/Desktop/allanwork/ArsenicProjectwithAllan/workon090909_eod")
require(foreign)
# masterdata <- read.spss("newWrkspsav.sav", to.data.frame = TRUE)
# write.csv(masterdata, "masterdata.csv")
# bigdata <- read.csv("bigdata2a.csv", sep = ",")
# bigdata2 <- cbind(bigdata, bmi = masterdata$BMI )
# workdata0 <- read.csv("workable.csv", sep = ",")
# workdata1 <- cbind(workdata, bmi1 = bigdata2$bmi)
# workdata <- workdata1[, c(2:length(workdata1))]
 cleandata <- read.csv("cleandata2a.csv", na.strings = "NA")
# select only those individuals who have complete records for
# diet, micronutrients, and methylation variables
# function to create tertiles and anova based p values for the tertiles
# cut bmi1 into three parts < 16.9, 16.9-19.3, and > 19.3
# create a variable with three levels of body mass index

# names(cleandata)
#length(cleandata$SUBJECT)
#table(cleandata$STATUS)
#length(cleandata$TOTAS)
#summary(cleandata$TOTAS)
#tertiles of total arsenic
t.totas <- quantile(cleandata$TOTAS, probs = seq(0,1,0.33), na.rm = T)
cleandata$ttotas <- cut(cleandata$TOTAS, breaks = c(-1, t.totas,max(cleandata$TOTAS, na.rm = T)))
# don't need the following table now, so comment it out:
# tbl.cdtotas <- table(cleandata$ttotas)

# write a function to accurately reflect the tertiles
# write the tertile function first
# the following is a useful function but we won't need that now
# we can open the vault when necessary
# comment out this function as well
################# function to comment out ##########
#goodtertle <- function(x){
#  gtt1 <- quantile(x, probs = seq(0,1,0.33), na.rm = T)
#  gtt2 <- cut(x, breaks = c((min(x, na.rm = T) - 1), gtt1, (max(x, na.rm = T)+1#)))
#  levels(gtt2) = c("Q1","Q1","Q2","Q3","Q3")
#  
#  return(gtt2)
#}
##################################################
# comment out the following as well
#gbmi <- goodtertle(cleandata$BMI)
#gtotas <- goodtertle(cleandata$TOTAS)
# ###
# we got BMI data in, but we are not going to use BMI for analyses,
# so commment out the following as well:
####
#summary(cleandata$TOTASAFS)
bmit <- quantile(cleandata$BMI, probs = seq(0,1,0.33), na.rm = T)
cleandata$bmit <- cut(cleandata$BMI, breaks = c(0,bmit,max(cleandata$BMI, na.rm = T)))
#table(cleandata$bmit)


#write.csv(workdata, "workdata.csv")
#workdata$bmid <- cut(workdata$bmi1, breaks = c( min(workdata$bmi1, na.rm = TRUE), 
#16.9,19.3, max(workdata$bmi1, na.rm = TRUE)))
#workdata$totalarsenic <- workdata$in.as + workdata$mma + workdata$dma
#levels(workdata$bmid) = c("low", "mid", "high")
# anova for bmi for inas%, mma% and dma%
#anovabmiinas <- aov(workdata$inaspct ~ workdata$bmid)
#anovabmimma <- aov(workdata$mmapct ~ workdata$bmid)
#anovabmidma <- aov(workdata$dmapct ~ workdata$bmid)

# means for bmi for inas%, mma%, dma%

#means and standard deviations for the dietary variables

# dataset1 <- bigdata2[, c(5:21,23:24, 39:45, 47:56)]

# develop the corresponding dataset
# first create the inorganic arsenic, mma, dma percentages, thus:
# first add tertiles of BMI to the dataset
# cleandata$bmit = goodtertle(cleandata$BMI)
cleandata$inpct = cleandata$INPROP * 100
cleandata$mmapct = cleandata$MMAPROP * 100
cleandata$dmapct = cleandata$DMAPROP * 100

# for clean data, the diet variabls are 9 to 28
# call calclean now, thus:
calclean <- function(x){ return(x/cleandata$DENERGY) }

# comment out other calorie adjust

# caladjust <- function(x){ return(x/dataset1$energy) }
# get a new clean, calorie adjusted for diet dataset here:

cleandatacal <- data.frame(sapply(cleandata[,c(9:14, 16:28)], calclean))

# comment out the old dataset
# datasetcal <- sapply(dataset1[, c(1:19)], caladjust)

# the diet and micronutrient variables are located between 9:28, and
# serum variables are between 43:62
# so the following dataset is a  composite dataset containing calorie adjusted
# dietary variables, micronutrients, and methylation information

dietmunutscaladj <- data.frame(cbind(cleandata[,c(1:7,15)], cleandatacal, cleandata[,c(42:76)]))
# the following dataset is about raw diet and micronutrients, not calorie adjusted
dietmunuts <- cleandata[,c(9:28, 42:62)]

# recreate the dataset with calorie adjustment

# get means and standard deviations of all variables

meanv <- sapply(dietmunuts, mean, na.rm = TRUE)
sdv <- sapply(dietmunuts, sd, na.rm = TRUE)
meansdv <- round(cbind(meanv, sdv), 3)

# this is for not calorie adjusted, use dietmunutscaladj for calorie adjusted
# comment out the following they are for old datasets, now obsolete
#meanvalues <- sapply(dataset1, mean, na.rm = TRUE)
#sdvalues <- sapply(dataset1, sd, na.rm = TRUE)
# meansdvalues <- cbind(meanvalues, sdvalues)



#meansbmiinas <- cbind(mean( workdata$inaspct[workdata$bmid == "low"], na.rm = TRUE),
  #                    mean( workdata$inaspct[workdata$bmid == "mid"], na.rm = TRUE),
   #                   mean(workdata$inaspct[workdata$bmid == "high"], na.rm = TRUE))

#meansbmimma <- cbind(mean( workdata$mmapct[workdata$bmid == "low"], na.rm = TRUE),
    #                 mean( workdata$mmapct[workdata$bmid == "mid"], na.rm = TRUE),
     #                 mean(workdata$mmapct[workdata$bmid == "high"], na.rm = TRUE))

#meansbmidma <- cbind(mean( workdata$dmapct[workdata$bmid == "low"], na.rm = TRUE),
      #               mean( workdata$dmapct[workdata$bmid == "mid"], na.rm = TRUE),
       #               mean(workdata$dmapct[workdata$bmid == "high"], na.rm = TRUE))

# now create tertiles of urinary total arsenic and do the same thing

# tertiles for total arsenic

# tertiletotas
## added 26-3-2010

mean.creat <- mean(cleandata$CREAT, na.rm = T)
sd.creat <- sd(cleandata$CREAT, na.rm = T)


tertotas <- function(x,y){
	ttotas1 <- cut(x, breaks = quantile(x, na.rm = TRUE, probs = seq(0,1,0.3)))
	levels(ttotas1) = c("low", "mid", "high")
	totas2 <- mean(y[ttotas1 == "low"], na.rm = TRUE)
	totas3 <- mean(y[ ttotas1 == "mid"], na.rm = TRUE)
	totas4 <- mean(y[ ttotas1 == "high"], na.rm = TRUE)
	totas5 <- cbind(totas2, totas3, totas4)
	pvalue = anova(lm(y ~ ttotas1))$"Pr(>F)"[1]
	finalout <- cbind(totas5, pvalue)
	return(finalout)
	}

## comment out the following function no use
#bmifunction <- function(x = workdata$bmid,y){
#	bmif1 <- cbind(	mean(y[x == "low"], na.rm = TRUE),
#	                mean(y[x == "mid"], na.rm = TRUE),
#	                mean(y[x == "high"], na.rm = TRUE))
#	pvalue <- anova(lm(y ~ x))$"Pr(>F)"[1]
#	finaloutbmi <- cbind(bmif1, pvalue)
#	return(finaloutbmi)
#	}
	
# Comment out the following three objects as well not needed now	                
	

#inastotalas <- tertotas(workdata$totalas, workdata$inaspct)
#mmatotalas <- tertotas(workdata$totalas, workdata$mmapct)
#dmatotalas <- tertotas(workdata$totalas, workdata$dmapct)     

#workdata$totaltertiles <- cut(workdata$totalas, 
#                              breaks = quantile(workdata$totalas, na.rm = TRUE, 
#                              probs = seq(0,1,0.3) ) ) 




# keep  the following is a good function but I am going to write a better one below
tertest <- function(x,y){
 tertest1 <- cut(x, breaks = quantile(x, na.rm = T, probs = seq(0,1,0.3)), 
                 labels = c("q1","q2","q3"))
 tertest2 <- t.test( y[tertest1 == "q1"], y[tertest1 == "q3"], paired = F)
 meanforq1 = mean(y[tertest1 == "q1"], na.rm = T)
 meanforq3 = mean(y[tertest1 == "q3"], na.rm = T)
 meandiff = meanforq3 - meanforq1
 
 finaldb <- cbind(meanforq1, meanforq3, meandiff, pvalue = round(tertest2$p.value, 3)) 
 
 return(finaldb)
}

## on 26-3-2010
# do an entry for creatining and the methylated metabolites
## we do not need that, because this creatinine is not correct,
## the correct one is one without any 0 value in it
## so comment out these ones
#creat.inpct <- tertest(cleandata$CREAT, cleandata$inpct)
#creat.mma <- tertest(cleandata$CREAT, cleandata$mmapct)
#creat.dma <- tertest(cleandata$CREAT, cleandata$dmapct)



# the following is an alternative function
# We are not going to use it now

goodtert2 <- function(x,y){
  tert1 <- cut(x, breaks = c((min(x, na.rm = T) - 1), quantile(x,na.rm = T, probs = seq(0,1,0.33)), (max(x, na.rm = T) + 1)), labels = c("q1","q1","q2","q3","q3"))
  ttest2 <- t.test( y[tert1 == "q1"], y[tert1 == "q3"], paired = F)
  meanq1 = mean(y[tert1 == "q1"], na.rm = T)
  meanq3 = mean(y[tert1 == "q3"], na.rm = T)
  mdiff = meanq3 - meanq1
  finals = cbind(meanq1, meanq3, mdiff, pvalue = round(ttest2$p.value, 3))
  return(finals)
}
  
# next, we are going to test it for inas, mma, dma
# we are going to use cleandata for this
# the following six sets of variables are related to
# one on one variable (these variables are adjusted for calorie)
# adjusted for calorie indicates that they are divided by calorie intake
# doing it using tertest

# inasvalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], tertest, cleandata$inpct)))
#inasval <- inasvalues[ order(-abs(inasvalues$X3)), ]

#mmavalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], tertest, cleandata$mmapct)))
#mmaval <- mmavalues[ order(-abs(mmavalues$X3)), ]

#dmavalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], tertest, cleandata$dmapct)))
#dmaval <- dmavalues[ order(-abs(dmavalues$X3)), ]

## analysis done on 26 march 2010
## comparison of diet and micronutrients with total arsenic
## on 27-03-2010, run with newdata
## the newdata1 is a totally clean dataset that does not contain
## any 0 value for creatinine or total arsenic
## plus it contains a new variable called totalarsenic where
## all zero values are removed

newdata1 <- read.csv("newdata.csv", sep = ",")


# totasvalues = data.frame(t(sapply(newdata1[, c(9:27, 43:62)], tertest, newdata1$totalarsenic)))
#totasval <- totasvalues[ order(-abs(totasvalues$X3)), ]

#write.table(totasval, file = "totasval1.csv", sep = ",")

## also, total arsenic with creatinine

#totas.creat <- tertest(newdata1$CREAT, newdata1$totalarsenic)

### get a spreadsheet like sprdsht2.xls

# spread27032010 <- newdata1[,c(8, 54, 45, 57, 28, 11, 41, 63)]
# write.table(spread27032010, file = "spreadsheet27march.csv", sep = ",")



#creatinine.val = data.frame(t(sapply(dietmunutscaladj[, c(9:27, 29:48)], tertest, cleandata$CREAT)))
#ordered.creat <- creatinine.val[ order(-abs(creatinine.val$X3)), ]

#write.table(ordered.creat, file = "creatval.csv", sep = ",")

## use the goodtert to see if there's any difference


#creatinine.val1 = data.frame(t(sapply(dietmunutscaladj[, c(9:27, 29:48)], goodtert2, cleandata$CREAT)))
#ordered.creat1 <- creatinine.val1[ order(-abs(creatinine.val1$X3)), ]

#write.table(ordered.creat1, file = "creatval2.csv", sep = ",")

## scatterplots of homocysteine (SHCYST), betacryptoxanthine (SBETACAR), methionine (SMET) with
## urine creatinine
## the following was deprecated because of 0 values so comment out
#attach(dietmunutscaladj)
#cor.hcyst.cr <- cor.test(SHCYST, CREAT)
#cor.bcrypt.cr <- cor.test(SBCRYPT, CREAT)
#cor.met.cr <- cor.test(SMET, CREAT)

#hcyst <- cleandata[,c(54,42)]
#bcr <- cleandata[,c(45,42)]
#met <- cleandata[,c(57,42)]

#spreadsheet2 <- dietmunutscaladj[,c(8,40,31,43,28,11,61)]
#write.table(spreadsheet2, file = "sprdsht2.csv", sep = ",")
# all the data were writtent to a spreadsheet
# now that it has been done, comment it out


# allserum <- dietmunutscaladj[,c(29:48)]

# Ok, import allserum into here,
# then replace the dietmunutscaladj serum variables with this one
# then run tables 3
# the micronutrient variables in allserum with 0 values are NA
# this dataset is now redundant
# allserum2 <- read.csv("allserum.csv", sep = ",", header = TRUE)
# new, recreated data
# this dataset has demographic socioeconomic data
# dietary variables all calorie adjusted
# serum micronutrients from which 0s are removed to NA
# the following dataset is redundant too
# newdata <- data.frame(cbind(dietmunutscaladj[,c(1:28,49:62)], allserum2))


## run the table 3 thing with new data
## the following code is run and outputs tabled
## comment it out 
# inasvalues.new = data.frame(t(sapply(newdata[, c(9:27, 43:62)], tertest, newdata$inpct)))
#inasval.new <- inasvalues.new[ order(-abs(inasvalues.new$X3)), ]

# mmavalues.new = data.frame(t(sapply(newdata[, c(9:27,43:62)], tertest, newdata$mmapct)))
# mmaval.new <- mmavalues.new[ order(-abs(mmavalues.new$X3)), ]

#dmavalues.new = data.frame(t(sapply(newdata[, c(9:27, 43:62)], tertest, newdata$dmapct)))
# dmaval.new <- dmavalues.new[ order(-abs(dmavalues.new$X3)), ]


#inasnew.cr1 <- tertest(newdata1$CREAT, newdata1$inpct)
#mmanew.cr1 <- tertest(newdata1$CREAT, newdata1$mmapct)
#dmanew.cr1 <- tertest(newdata1$CREAT, newdata1$dmapct)

#write.table(inasval.new, file = "inasvalnew.csv", sep = ",")
#write.table(mmaval.new, file = "mmavalnew.csv", sep = ",")
#write.table(dmaval.new, file = "dmavalnew.csv", sep = ",")


#### the following code is of no use now
## comment it out
#spread1 <- cleandata[,c(1,54,45,57,42,15 )]
#write.table(spread1, file = "spread1.csv", sep = ",")
### no use because it has micronuttient data 0

## the following plots still may be called for
## in that case attach newdata to the space and run the codes
## otherwise for now comment them out

# attach(newdata)
#plot(SHCYST, CREAT, main = "Scatterplot of homocysteine with creatinine", xlab = "Plasma homocysteine", ylab = "Urine creatinine")

#plot(SBCRYPT, CREAT, main = "Scatterplot of betacryptoxanthine with creatinine", xlab = "Plasma betacryptoxanthine", ylab = "Urine creatinine")

#plot(SMET, CREAT,main = "Scatterplot of methionine with creatinine", xlab = "Serum methionine", ylab = "Urine creatinine" )

#plot(log(SHCYST), log(CREAT),main = "Scatterplot of logs of  homocysteine with logs of creatinine", xlab = "log (Plasma homocysteine)", ylab = "log (Urine creatinine)" )

#plot(log(SBCRYPT), log(CREAT),main = "Scatterplot of logs of betacryptoxanthine with logs of creatinine", xlab = "log(Plasma betacryptoxanthine)", ylab = "log(Urine creatinine)" )

#plot(log(SMET), log(CREAT),main = "Scatterplot of logs of methionine with logs of creatinine", xlab = "log(Serum methionine)", ylab = "log(Urine creatinine)" )

#detach(newdata)

### newdata is more accurate reflecting necessary variables
## newdata was created on 26-03-2010 following allan's suggestions

## association between total arsenic and creatinine
## the following is fine but not needed now, so comment out

#total.cr <- tertest(cleandata$CREAT, cleandata$TOTAS)
#cr.total <- tertest(cleandata$TOTAS, cleandata$CREAT)
#cor.cr.tot <- cor.test(cleandata$CREAT, cleandata$TOTAS)
#####
### the following codes are no longer needed ###
# these are the old estimates, may be deprecated (although new tests give identical estimates)            
# ttestinas <- data.frame(t(sapply(workdata[,c( 3:34, 36:53)], tertest, workdata$inaspct)))
# ttestinas1 <- ttestinas[ order(-abs(ttestinas$X3)), ]
# ttestmma <- data.frame(t(sapply(workdata[,c(3:34, 36:53)], tertest, workdata$mmapct)))
#ttestmma1 <- ttestmma[ order(-abs(ttestmma$X3)), ]
#### these chunks of codes need to be removed ###

# ttestdma <- data.frame(t(sapply(workdata[,c(3:34, 36:53)], tertest, workdata$dmapct)))
# ttestdma1 <- ttestdma[ order(-abs(ttestdma$X3)), ]

# function to create tertiles and cross tabulate x and y
## the following function creates tertiles of two variables
## then conducts a spearman correlation
## this may still be needed, so evaluate

tertilecompare <- function(x,y){
	tertx <- cut(x, breaks = quantile(x, na.rm = TRUE, probs = seq(0,1,0.3)), 
	             labels = c("qx1","qx2","qx3") )
	terty <- cut(y, breaks = quantile(y, na.rm = TRUE, probs = seq(0,1,0.3)), 
	             labels = c("qy1","qy2","qy3") )
	twocor <- cor(tertx, terty, use = "all.obs")
	return(summary(twocor))
	}

# Plan of multivariate analysis

### the following set of models are not necessary ###
# they need to be removed otherwise confusing

##### models to be removed from space comment out ###
# model-1: inas ~ lyco+riboflavin+age+gender+housing+educ+
#anfat3 <- cut(workdata$Fatanimal, 
#breaks = quantile(workdata$Fatanimal, na.rm = T, probs = seq(0,1,0.3),
#levels = c("1","2","3") ) )	

#retinol3 <- cut(workdata$Retinol, 
#breaks = quantile(workdata$Retinol, na.rm = T, probs = seq(0,1,0.3),
#levels = c("1","2","3") ) )	

# cross tabulations between tertiles of animal fat and plasma retinol
# may still be useful, keep
tablefunction <- function(x,y){
	t1 <- table(x,y)
	t2 <- round(prop.table(t1,1)*100, 2)
	t3 <- data.frame(cbind(t1[,1], t2[,1], t1[,2], t2[,2], t1[,3], t2[,3]) )
	return(t3)
	}
	
# put out only those rows out of ttestinas1, ttestmma1, and ttestdma1
# that have p-values less than 0.05	

### comment out
#ttestinassig <- ttestinas1[ttestinas1$X4 < 0.05, ]
#ttestmmasig <- ttestmma1[ttestmma1$X4 < 0.05, ]
#ttestdmasig <- ttestdma1[ttestdma1$X4 < 0.05, ]
####
# Get pearson correlation matrix of all the different dietary variables
# and serum micronutrients
## comment out the following, old deprecated
#cordf <- data.frame(cbind(workdata$Lycopene, workdata$riboflavin, 
 #                        workdata$Selen, workdata$LutZea, workdata$Folate))
#inascorrelations <- cor(cordf, method = "pearson", use = "pairwise.complete.obs")

#cordfmma <- data.frame(cbind(workdata$VitA, workdata$Fatanimal, workdata$Retinol, 
 #                      workdata$Homocyst, workdata$Folate, workdata$lysine,
  #                     workdata$Selen, workdata$Proteinan, workdata$tryptophan,
  #                     workdata$Vit.B6, workdata$phosph))

#mmacormatrix <- cor(cordfmma, method = "pearson", use = "pairwise.complete.obs")

#cordfdma <- data.frame(cbind(workdata$riboflavin, workdata$Lycopene, 
                       workdata$lysine, workdata$LutZea))
#dmacormatrix <- cor(cordfdma, method = "pearson", use = "pairwise.complete.obs")

### the following function is essential for setting table 4
## this function creates tertiles of variables, evaluate

createtertile <- function(x){
	tertilex1 <- cut(x, breaks = quantile(x, na.rm = T, probs = seq(0,1,0.3)))}
# use the tertile function to run studies
# dataset: dietmunutscaladj
# variables: inas - lycopene, riboflavin, retinol, selenium, lutein, folate, b6
# variables: mma ~ retinol, animal fat, pretinol, homocysteine, folate, selenium, animal protein, b6, phosph, alphatoco, carbohydrate, lycopene, fiber
# variables: dma ~ riboflavin, lycopene, lutien, carb, cholesterol, selenium, alphatoc,

### List of multivariate models begin here ###
## revise the models again
### models begin here 

# for inpct, start with 1. riboflavin + folate, then add the others
## the following lists new models based on cleaned up data

inas0 <- lm(inpct ~ AGE + GENDER + EDUC + HOUSING + STATUS 
         , data = newdata1)

inas0c <- summary(inas0)$coefficient
inas0r <- summary(inas0)$r.squared

inas1 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING + STATUS 
         , data = newdata1)
inas1c <- summary(inas1)$coefficients
inas1r <- summary(inas1)$r.squared


inas2 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SSEL) +
             createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
inas2c <- summary(inas2)$coefficient
inas2r <- summary(inas2)$r.squared

inas3 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SFOL) +
             + createtertile(SLYCO) + createtertile(SSEL) + AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
inas3c <- summary(inas3)$coefficient
inas3r <- summary(inas3)$r.squared

## now mma
#### start with mma%
# start with dietary animal fat and plasma retinol
# add homocysteine and then take away
# add folate to it
# then add selenium
# then keep the animal fat + plasma retinol + folate + selenium and
# keep adding to it

mma0 <-  lm(mmapct ~ AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma0coef <- summary(mma0)$coefficient
mma0r <- summary(mma0)$r.squared

  
mma1 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma1coef <- summary(mma1)$coefficient
mma1r <- summary(mma1)$r.squared

mma2 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SHCYST) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma2coef <- round(summary(mma2)$coefficients, 2)
mma2r <- summary(mma2)$r.squared

mma3 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SHCYST)+ createtertile(DANPROT) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma3coef <- round(summary(mma3)$coefficient, 3)
mma3r <- summary(mma3)$r.squared

mma4 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SHCYST)+ createtertile(SFOL) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma4coef <- round(summary(mma4)$coefficient, 3)
mma4r <- round(summary(mma4)$r.squared, 3)

mma5 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SFOL) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma5coef <- round(summary(mma5)$coefficients, 3)
mma5r <- round(summary(mma5)$r.squared, 3)


mma6 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SFOL) + createtertile(SSEL) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma6coef <- round(summary(mma6)$coefficient, 3)
mma6r <- round(summary(mma6)$r.squared, 3)

mma7 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SSEL) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma7coef <- round(summary(mma7)$coefficient, 3)
mma7r <- round(summary(mma7)$r.squared, 3)

mma8 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SSEL) + createtertile(SATOC) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma8coef <- round(summary(mma8)$coefficient, 3)
mma8r <- round(summary(mma8)$r.squared, 3)


mma9 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SSEL) + createtertile(SBCRYPT) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma9coef <- summary(mma8)$coefficient
mma9r <- summary(mma9)$r.squared

## in the following model, we start with selenium


mmaSel <- lm(mmapct ~ createtertile(SSEL) +
             createtertile(DFATANIM) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mmaSelcoef <- round(summary(mmaSel)$coefficient, 3)
mmaSelr <- round(summary(mmaSel)$r.squared, 3)


mmaSel2 <- lm(mmapct ~ createtertile(SSEL) +
             createtertile(DFATANIM) + createtertile(SRET) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mmaSel2coef <- round(summary(mmaSel2)$coefficients, 3)
mmaSel2r <- round(summary(mmaSel2)$r.squared, 3)


mmaSel3 <- lm(mmapct ~ createtertile(SSEL) +
             createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SHCYST) + 
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mmaSel3coef <- round(summary(mmaSel3)$coefficients, 3)
mmaSel3r <- round(summary(mmaSel3)$r.squared, 3)

mmaSel4 <- lm(mmapct ~ createtertile(SSEL) +
             createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(DRETINOL) + 
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mmaSel4coef <- round(summary(mmaSel4)$coefficient, 3)
mmaSel4r <- round(summary(mmaSel4)$r.squared, 3) 

mmaSel5 <- lm(mmapct ~ createtertile(SSEL) +
             createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SFOL) + 
             AGE +
             GENDER + EDUC + HOUSING, data= newdata1)
mmaSel5coef <- round(summary(mmaSel5)$coefficient, 3)
mmaSel5r <- round(summary(mmaSel5)$r.squared, 3)

### dma

dma1 <- lm(dmapct ~ createtertile(DRIBOFL) + createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
dma1coef <- round(summary(dma1)$coefficient, 3)
dma1r <- round(summary(dma1)$r.squared, 3)

dma2 <-  lm(dmapct ~ createtertile(DRIBOFL) + createtertile(SLYCO) + createtertile(SLUT)
             + AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
dma2coef <- round(summary(dma2)$coefficient, 3)
dma2r <- round(summary(dma2)$r.squared, 3)

#newdata$totalarsenic <- (newdata$INAS + newdata$MMA + newdata$DMA)

#write.table(newdata, file = "newdata.csv", sep = ",")


#### Because urine creatinine is so strong, therefore we run additional models
### in these models, urine creatinine will be added as covariates
### we shall then examine
### copy all the models from above
### add .cr to the model name

inas0.cr <- lm(inpct ~ createtertile(CREAT) +
                       createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
inas0crcoef <- round(summary(inas0.cr)$coefficient, 3)
inas0crR <- round(summary(inas0.cr)$r.squared, 3)

inas1.cr <- lm(inpct ~ createtertile(CREAT) +
                       createtertile(DRIBOFL) + createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
inas1crcoef <- round(summary(inas1.cr)$coefficient, 3)
inas1crR <- round(summary(inas1.cr)$r.squared, 3)

inas2.cr <- lm(inpct ~ createtertile(CREAT) +
                       createtertile(DRIBOFL) + createtertile(SSEL) +
             createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
inas2crcoef <- round(summary(inas2.cr)$coefficient, 3)
inas2crR <- round(summary(inas2.cr)$r.squared, 3) 

inas3.cr <- lm(inpct ~ createtertile(CREAT) +
                       createtertile(DRIBOFL) +
                       createtertile(SFOL) +
                       createtertile(SLYCO) +
                       createtertile(SSEL) + AGE +
             GENDER + EDUC + HOUSING + STATUS, data= newdata1)
inas3crcoef <- round(summary(inas3.cr)$coefficient, 3)
inas3crR <- round(summary(inas3.cr)$r.squared, 3)

#### new models with mma and creatinine in there
mma1.cr <- lm(mmapct ~ createtertile(CREAT) +
                                         createtertile(DFATANIM) + createtertile(SRET) +
                                        + AGE +
                                        GENDER + EDUC + HOUSING +
                                        STATUS, data= newdata1)

mma1crcoef <- round(summary(mma1.cr)$coefficient,3)
mma1crR <- round(summary(mma1.cr)$r.squared, 3)

mma2.cr <- lm(mmapct ~  createtertile(CREAT) + createtertile(DFATANIM) +
                                          createtertile(SRET) +
                                         + createtertile(SHCYST) +
                                           AGE +
                                         GENDER + EDUC + HOUSING +STATUS, data= newdata1)
mma2crcoef <- round(summary(mma2.cr)$coefficient, 3)
mma2crR <- round(summary(mma2.cr)$r.squared, 3)

mma3.cr <- lm(mmapct ~ createtertile(CREAT) +
                                          createtertile(DFATANIM) + createtertile(SRET) +
                                           + createtertile(SHCYST)+ createtertile(DANPROT) +
                                             AGE +
                                            GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma3crcoef <- round(summary(mma3.cr)$coefficient, 3)
mma3crR <- round(summary(mma3.cr)$r.squared, 3)

mma4.cr <- lm(mmapct ~ createtertile(CREAT) + createtertile(DFATANIM) +
                                        createtertile(SRET) +
                                       + createtertile(SHCYST)+ createtertile(SFOL) +
                                          AGE +
                                     GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma4crcoef <- round(summary(mma4.cr)$coefficient, 3)
mma4crR <- round(summary(mma4.cr)$r.squared, 3)

mma5.cr <- lm(mmapct ~ createtertile(CREAT) +
                                        createtertile(DFATANIM) + createtertile(SRET) +
                                        + createtertile(SFOL) +
                                          AGE +
                                         GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma5crcoef <- round(summary(mma5.cr)$coefficient, 3)
mma5crR <- round(summary(mma5.cr)$r.squared, 3)

mma6.cr <- lm(mmapct ~ createtertile(CREAT) +
                                          createtertile(DFATANIM) + createtertile(SRET) +
                                          + createtertile(SFOL) + createtertile(SSEL) +
                                          AGE +
                                         GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma6crcoef <- round(summary(mma6.cr)$coefficient, 3)
mma6crR <- round(summary(mma6.cr)$r.squared, 3)

mma7.cr <- lm(mmapct ~  createtertile(CREAT) +
                                          createtertile(DFATANIM) + createtertile(SRET) +
                                         + createtertile(SSEL) +
                                          AGE +
                                           GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma7crcoef <- round(summary(mma7.cr)$coefficient, 3)
mma7crR <- round(summary(mma7.cr)$r.squared, 3)

mma8.cr <- lm(mmapct ~ createtertile(CREAT) +
                           createtertile(DFATANIM) + createtertile(SRET) +
                              + createtertile(SSEL) + createtertile(SATOC) +
                               AGE +
                               GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma8crcoef <- round(summary(mma8.cr)$coefficient, 3)
mma8crR <- round(summary(mma8.cr)$r.squared, 3)

mma9.cr <- lm(mmapct ~ createtertile(CREAT) +
                                         createtertile(DFATANIM) + createtertile(SRET) +
                                         + createtertile(SSEL) + createtertile(SBCRYPT) +
                                           AGE +
                                         GENDER + EDUC + HOUSING + STATUS, data= newdata1)
mma9crcoef <- round(summary(mma9.cr)$coefficient, 3)
mma9crR <- round(summary(mma9.cr)$r.squared, 3)

### dma models here ###

dma1.cr <- lm(dmapct ~ createtertile(CREAT) +
                                       createtertile(DRIBOFL) + createtertile(SLYCO) +
                                     AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
dma1crcoef <- round(summary(dma1.cr)$coefficient, 3)
dma1crR <- round(summary(dma1.cr)$r.squared, 3)

dma2.cr <-  lm(dmapct ~ createtertile(CREAT) +
                                         createtertile(DRIBOFL) + createtertile(SLYCO) +
                                         createtertile(SLUT)
                                       + AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
dma2crcoef <- round(summary(dma2.cr)$coefficient, 3)
dma2crR <- round(summary(dma2.cr)$r.squared, 3)

### what happens if we do not have any nutritional factors in ###


inas00 <- lm(inpct ~  AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
inas00coef <- round(summary(inas00)$coefficient, 3)
inas00R <- round(summary(inas00)$r.squared, 3)

mma00 <- lm(mmapct ~ AGE +
                                        GENDER + EDUC + HOUSING +
                                        STATUS, data= newdata1)

mma00coef <- round(summary(mma00)$coefficient,3)
mma00R <- round(summary(mma00)$r.squared, 3)


dma00 <- lm(dmapct ~ AGE + GENDER + EDUC + HOUSING + STATUS, data = newdata1)
dma00coef <- round(summary(dma00)$coefficient, 3)
dma00R <- round(summary(dma00)$r.squared, 3)







##### models end here

save.image("work28032010middday.Rdata")

















