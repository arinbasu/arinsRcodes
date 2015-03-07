setwd("C:/Users/arinbasu/Documents/Projects/ArsenicProjectwithAllan/workon090909_eod")
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
tbl.cdtotas <- table(cleandata$ttotas)

# write a function to accurately reflect the tertiles
# write the tertile function first
goodtertle <- function(x){
  gtt1 <- quantile(x, probs = seq(0,1,0.33), na.rm = T)
  gtt2 <- cut(x, breaks = c((min(x, na.rm = T) - 1), gtt1, (max(x, na.rm = T)+1)))
  levels(gtt2) = c("Q1","Q1","Q2","Q3","Q3")
  
  return(gtt2)
}

gbmi <- goodtertle(cleandata$BMI)
gtotas <- goodtertle(cleandata$TOTAS)

summary(cleandata$TOTASAFS)
bmit <- quantile(cleandata$BMI, probs = seq(0,1,0.33), na.rm = T)
cleandata$bmit <- cut(cleandata$BMI, breaks = c(0,bmit,max(cleandata$BMI, na.rm = T)))
table(cleandata$bmit)


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
dietmunuts <- cleandata[,c(9:28, 43:62)]

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

creat.inpct <- tertest(cleandata$CREAT, cleandata$inpct)
creat.mma <- tertest(cleandata$CREAT, cleandata$mmapct)
creat.dma <- tertest(cleandata$CREAT, cleandata$dmapct)



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

inasvalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], tertest, cleandata$inpct)))
inasval <- inasvalues[ order(-abs(inasvalues$X3)), ]

mmavalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], tertest, cleandata$mmapct)))
mmaval <- mmavalues[ order(-abs(mmavalues$X3)), ]

dmavalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], tertest, cleandata$dmapct)))
dmaval <- dmavalues[ order(-abs(dmavalues$X3)), ]

## analysis done on 26 march 2010
## comparison of diet and micronutrients with total arsenic

totasvalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], tertest, cleandata$TOTAS)))
totasval <- totasvalues[ order(-abs(totasvalues$X3)), ]

write.table(totasval, file = "totasval.csv", sep = ",")




# these are the old estimates, may be deprecated (although new tests give identical estimates)            
# ttestinas <- data.frame(t(sapply(workdata[,c( 3:34, 36:53)], tertest, workdata$inaspct)))
# ttestinas1 <- ttestinas[ order(-abs(ttestinas$X3)), ]
# ttestmma <- data.frame(t(sapply(workdata[,c(3:34, 36:53)], tertest, workdata$mmapct)))
#ttestmma1 <- ttestmma[ order(-abs(ttestmma$X3)), ] 

# ttestdma <- data.frame(t(sapply(workdata[,c(3:34, 36:53)], tertest, workdata$dmapct)))
# ttestdma1 <- ttestdma[ order(-abs(ttestdma$X3)), ]

# function to create tertiles and cross tabulate x and y

tertilecompare <- function(x,y){
	tertx <- cut(x, breaks = quantile(x, na.rm = TRUE, probs = seq(0,1,0.3)), 
	             labels = c("qx1","qx2","qx3") )
	terty <- cut(y, breaks = quantile(y, na.rm = TRUE, probs = seq(0,1,0.3)), 
	             labels = c("qy1","qy2","qy3") )
	twocor <- cor(tertx, terty, use = "all.obs")
	return(summary(twocor))
	}

Plan of multivariate analysis

##### models
# model-1: inas ~ lyco+riboflavin+age+gender+housing+educ+


anfat3 <- cut(workdata$Fatanimal, 
breaks = quantile(workdata$Fatanimal, na.rm = T, probs = seq(0,1,0.3),
levels = c("1","2","3") ) )	

retinol3 <- cut(workdata$Retinol, 
breaks = quantile(workdata$Retinol, na.rm = T, probs = seq(0,1,0.3),
levels = c("1","2","3") ) )	

# cross tabulations between tertiles of animal fat and plasma retinol

tablefunction <- function(x,y){
	t1 <- table(x,y)
	t2 <- round(prop.table(t1,1)*100, 2)
	t3 <- data.frame(cbind(t1[,1], t2[,1], t1[,2], t2[,2], t1[,3], t2[,3]) )
	return(t3)
	}
	
# put out only those rows out of ttestinas1, ttestmma1, and ttestdma1
# that have p-values less than 0.05	

# 
ttestinassig <- ttestinas1[ttestinas1$X4 < 0.05, ]
ttestmmasig <- ttestmma1[ttestmma1$X4 < 0.05, ]
ttestdmasig <- ttestdma1[ttestdma1$X4 < 0.05, ]

# Get pearson correlation matrix of all the different dietary variables
# and serum micronutrients

cordf <- data.frame(cbind(workdata$Lycopene, workdata$riboflavin, 
                         workdata$Selen, workdata$LutZea, workdata$Folate))
inascorrelations <- cor(cordf, method = "pearson", use = "pairwise.complete.obs")

cordfmma <- data.frame(cbind(workdata$VitA, workdata$Fatanimal, workdata$Retinol, 
                       workdata$Homocyst, workdata$Folate, workdata$lysine,
                       workdata$Selen, workdata$Proteinan, workdata$tryptophan,
                       workdata$Vit.B6, workdata$phosph))

mmacormatrix <- cor(cordfmma, method = "pearson", use = "pairwise.complete.obs")

cordfdma <- data.frame(cbind(workdata$riboflavin, workdata$Lycopene, 
                       workdata$lysine, workdata$LutZea))
dmacormatrix <- cor(cordfdma, method = "pearson", use = "pairwise.complete.obs")

createtertile <- function(x){
	tertilex1 <- cut(x, breaks = quantile(x, na.rm = T, probs = seq(0,1,0.3)))}
# use the tertile function to run studies
# dataset: dietmunutscaladj
# variables: inas - lycopene, riboflavin, retinol, selenium, lutein, folate, b6
# variables: mma ~ retinol, animal fat, pretinol, homocysteine, folate, selenium, animal protein, b6, phosph, alphatoco, carbohydrate, lycopene, fiber
# variables: dma ~ riboflavin, lycopene, lutien, carb, cholesterol, selenium, alphatoc,

### List of multivariate models begin here ###
## revise the models again


# for inpct, start with 1. riboflavin + folate, then add the others
new.m1 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING + STATUS 
         , data = dietmunutscaladj)
m1coeff <- summary(new.m1)$coefficients

new.m1nocr <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SLYCO) +
                 AGE + GENDER + EDUC + HOUSING +
                 STATUS, data = dietmunutscaladj)
m1nocoeff <- summary(new.m1nocr)$coefficients

new.m2 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SFOL) +
             createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING + STATUS, data = dietmunutscaladj)
newm2coef <- summary(new.m2)$coefficients

new.m3 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SFOL) +
             + createtertile(SLYCO) + createtertile(DRETINOL) + AGE +
             GENDER + EDUC + HOUSING + STATUS, data= dietmunutscaladj)
newm3coef <- summary(new.m3)$coefficients

new.m4 <- lm(inpct ~ createtertile(DRIBOFL) +
             createtertile(SFOL) + createtertile(SLYCO)
             + createtertile(SSEL) + AGE +
             GENDER + EDUC + HOUSING + STATUS, data= dietmunutscaladj)
newm4coef <- summary(new.m4)$coefficients

new.m5 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SFOL) +
             + createtertile(SLUT) + AGE +
             GENDER + EDUC + HOUSING + STATUS, data= dietmunutscaladj)
newm5coef <- summary(new.m5)$coefficients

new.m6 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SFOL) +
             + createtertile(DVITB6) + AGE +
             GENDER + EDUC + HOUSING + STATUS, data= dietmunutscaladj)
newm6coef <- summary(new.m6)$coefficients

#### start with mma%
# start with dietary animal fat and plasma retinol
# add homocysteine and then take away
# add folate to it
# then add selenium
# then keep the animal fat + plasma retinol + folate + selenium and
# keep adding to it
m.mma1 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SHCYST) + AGE +
             GENDER + EDUC + HOUSING + STATUS + TOTAS, data= dietmunutscaladj)
mma1coef <- summary(m.mma1)$coefficients

m.mma2 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SHCYST) + createtertile(SFOL)+
             AGE +
             GENDER + EDUC + HOUSING + STATUS + TOTAS, data= dietmunutscaladj)
mma2coef <- summary(m.mma2)$coefficients


m.mma3 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SFOL)+
             AGE +
             GENDER + EDUC + HOUSING + STATUS + TOTAS, data= dietmunutscaladj)
mma3coef <- summary(m.mma3)$coefficients

######
m.mma4 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             + createtertile(SFOL)+ createtertile(SSEL) +
             AGE +
             GENDER + EDUC + HOUSING + STATUS, data= dietmunutscaladj)
mma4coef <- summary(m.mma4)$coefficients

## just write the above four models
## then for dma
m.dma1 <- lm(dmapct ~ createtertile(DRIBOFL) + createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING + STATUS, data = dietmunutscaladj)
mdma1coef <- summary(m.dma1)$coefficients

m.dma2 <-  lm(dmapct ~ createtertile(DRIBOFL) + createtertile(SLYCO) + createtertile(SLUT)
             + AGE + GENDER + EDUC + HOUSING + STATUS, data = dietmunutscaladj)
mdma2coef <- summary(m.dma2)$coefficients


m.dma2 <-  lm(dmapct ~ createtertile(DRIBOFL) + createtertile(SLYCO) +
              createtertile(SLUT) + createtertile(SSEL) + 
             + AGE + GENDER + EDUC + HOUSING + STATUS, data = dietmunutscaladj)
mdma2coef <- summary(m.dma2)$coefficients




m.dma3 <-  lm(dmapct ~ createtertile(DRIBOFL) + createtertile(SLYCO) +
              + createtertile(SSEL) + 
             + AGE + GENDER + EDUC + HOUSING + STATUS, data = dietmunutscaladj)
mdma3coef <- summary(m.dma3)$coefficients








# updated models, type one model, test, include subsequent variable
model1 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SLYCO) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)
model1coeff <- summary(model1)$coefficients

model1x <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SLYCO) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS, data = dietmunutscaladj)
model1xcoeff <- summary(model1x)$coefficients


# riboflavin held, lycopene dropped out
model2 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(DRETINOL) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)
model2coeff <- summary(model2)$coefficients

model2 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(DRETINOL) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)
model2coeff <- summary(model2)$coefficients


# neither riboflavin nor retinol was significant anymore

model3 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SSEL) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)
model3coeff <- summary(model3)$coefficients

# riboflavin had borderline significance but selenium drops out


model4 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SLUT) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)
model4coeff <- summary(model4)$coefficients

# riboflavin again becomes statistically significant but lutein drops out


model5 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SFOL) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)
model5coeff <- summary(model5)$coefficients

# both folate and riboflavin have borderline significance so they stay


model6 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SFOL) +
             + createtertile(DVITB6) + AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model6coeff <- summary(model6)$coefficients

# both folate and riboflavin become significant
# summary, riboflavin and serum folate are independently associated
# riboflavin is negatively associated with InAs% and
# folate is positively associated with InAs%


model7 <- lm(mmapct ~ createtertile(DRETINOL) + createtertile(DFATANIM) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model7coeff <- summary(model7)$coefficients

# dietary retinol drops out, dietary animal fat stays

model7a <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model7acoeff <- summary(model7a)$coefficients



model8 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             createtertile(SHCYST) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model8coeff <- summary(model8)$coefficients

# dietary animal fat and homocysteine both stay


model9 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET)+
             createtertile(SHCYST) +
             createtertile(SFOL) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model9coeff <- summary(model9)$coefficients

# dietary animal fat, homocysteine and folate are all significant and stay


model10 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
              createtertile(SHCYST) +
             createtertile(SFOL) + createtertile(SSEL) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model10coeff <- summary(model10)$coefficients

# homocysteine drops out but dietary animal fat, folate, and selenium stay


model11 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
              createtertile(SSEL) + createtertile(DANPROT) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model11coeff <- summary(model11)$coefficients

# animal protein and folate drop out animal fat and selenium stay

model12 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
              createtertile(SSEL) + createtertile(DVITB6) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model12coeff <- summary(model12)$coefficients

# dietary animal fat and serum selenium stay, vitamin B6 drops out


model13 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
              createtertile(SSEL) + createtertile(DPHOSPH) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model13coeff <- summary(model13)$coefficients

# dietary phosphate stays and selenium and dietary animal fat (barely) stay


model14 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +                                createtertile(DPHOSPH) + createtertile(SSEL) +
              createtertile(SATOC) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model14coeff <- summary(model14)$coefficients

# alphatocopherol drops off, animal fat and selenium stay


model15 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
              createtertile(SSEL) + createtertile(DCARB) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model15coeff <- summary(model15)$coefficients

# dietary carbohydrate drops out, animal fat, selenium stay


model16 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) + 
              createtertile(SSEL) + createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model16coeff <- summary(model16)$coefficients

# lycopene drops out but animal fat and selenium stay


model17 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
              createtertile(SSEL) + createtertile(DFIBRE) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model17coeff <- summary(model17)$coefficients

# animal fat and selenium stay, fibre drops out
#in summary, for MMA%, dietary animal fat and serum selenium were
# the most statistically significant and independently associated
# variables

model18 <- lm(dmapct ~ createtertile(DRIBOFL) + 
              createtertile(SLYCO) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model18coeff <- summary(model18)$coefficients

# riboflavin stays


model19 <- lm(dmapct ~ createtertile(DRIBOFL) + 
              createtertile(SLUT) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model19coeff <- summary(model19)$coefficients
# riboflavin stays

model20 <- lm(dmapct ~ createtertile(DRIBOFL) + 
              createtertile(DCARB) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model20coeff <- summary(model20)$coefficients

# riboflavin stays

model21 <- lm(dmapct ~ createtertile(DRIBOFL) + 
              createtertile(SCHOL) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model21coeff <- summary(model21)$coefficients

# riboflavin borderline but stays

model22 <- lm(dmapct ~ createtertile(DRIBOFL) + 
              createtertile(SSEL) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model22coeff <- summary(model22)$coefficients

# riboflavin borderline


model23 <- lm(dmapct ~ createtertile(DRIBOFL) + 
              createtertile(SATOC) +
             AGE + GENDER + EDUC + HOUSING +
             STATUS + TOTAS + BMI, data = dietmunutscaladj)
model23coeff <- summary(model23)$coefficients

# riboflavin borderline
# Overall, riboflavin was significantly and independently associated with dma


### list of multivariate models end here ###

model1noBMI <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SLYCO) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS, data = dietmunutscaladj)

model2 <- lm(inpct ~ createtertile(DRIBOFL) + createtertile(SLYCO) +
             + createtertile(SSEL) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)

model2 <- lm(mmapct ~ createtertile(DRETINOL) + createtertile(DFATANIM) +
             AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)

model3 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) +
             AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)

model4 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) + createtertile(SHCYST) +
             AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)

model5 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) + createtertile(SHCYST) +
             createtertile(SFOL) + AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)

model6 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) + 
             createtertile(SFOL) + createtertile(SSEL)+ AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)

model7 <- lm(mmapct ~ createtertile(DFATANIM) + createtertile(SRET) + 
             createtertile(DANPROT) + createtertile(SSEL)+ AGE + GENDER + EDUC + HOUSING + STATUS + TOTAS + BMI, data = dietmunutscaladj)


	
# create a further workable data

mvworkdata <- workdata[,c(3:21,22:34,37:53,55,61,74,75,76,80,81,82,83, 84,85)]

# the following dataset will be used for regression analysis
prepmvdata <- cbind(sapply(mvworkdata[,c(1:49)], createtertile), 
                           mvworkdata[,c(50:60)])	

datatertiles <- sapply(mvworkdata[,c(1:49)], createtertile)

levels(prepmvdata$Vit.B12) = c("Q3","Q2","Q1")

mvworkdata$locob <- mvworkdata$Vit.B12 < 185

prepmvdatalowfol <- subset(prepmvdata, Folate == "(0.8,2.2]")
prepmvdatamidfol <- subset(prepmvdata, Folate == "(2.2,3.1]")
prepmvdatahifol <- subset(prepmvdata, Folate == "(3.1,5.6]")

# ttestinasforlocob 
locobinas <- t.test(mvworkdata$inaspct ~ mvworkdata$locob)
locobmma <- t.test(mvworkdata$mmapct ~ mvworkdata$locob)
locobdma <- t.test(mvworkdata$dmapct ~ mvworkdata$locob)

# mma%

cormatrixforinas <- cor(mvworkdata[,c( 46,14,12,37,59 ) ], 
                    use = "pairwise.complete.obs")

cormatrixformma <- cor(mvworkdata[,c(12,3,42,41,36,37,1,35 )],
                         use = "pairwise.complete.obs")

cormatrixfordma <- cor(mvworkdata[,c( 14,46,44 )], 
                      use = "pairwise.complete.obs" )

# vitamin A can be combined with retinol, homocysteine, folate
# selen, tryptophan, vit.B6
# based on the mmamatrix data, here are the regression equations


# impressioins
# riboflavin associated with dma
# 

write.table(cormatrixforinas, file = "matrixinas.txt", sep = "\t")
write.table(cormatrixformma, file = "matrixmma.txt", sep = "\t")
write.table(cormatrixfordma, file = "matrixdma.txt", sep = "\t")

# lists of models, afer this set of models, remove the previous ones

model1 <- lm(inaspct ~ Lycopene + riboflavin + 
             agerec + gender + educ + house + bmi1 + totalas + Status.1,
              data = prepmvdata)
model2 <- lm(inaspct ~ Lycopene + riboflavin + Selen +
             agerec + gender + educ + house + bmi1 + totalas + Status.1,
              data = prepmvdata)
model3 <- lm(inaspct ~ Lycopene + VitA +
             agerec + gender + educ + house + bmi1 + totalas + Status.1,
              data = prepmvdata)

modelFatvita <- lm(mmapct ~ VitA + Fatanimal + 
             agerec + gender + educ + house + bmi1 + totalas + Status.1,
              data = prepmvdata)

model9 <- lm(mmapct ~ Fatanimal + Retinol + 
             agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model10 <- lm(mmapct ~ Fatanimal + Retinol + Homocyst + 
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

mmafatretfol <- lm(mmapct ~ Fatanimal + Retinol + Folate + 
               agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

mmafatretsel <- lm(mmapct ~ Fatanimal + Retinol + Folate + Selen +
               agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

mmafatretselpro <- lm(mmapct ~ Fatanimal + Retinol + Selen + Proteinan +
               agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)


modelFatprotein <- lm(mmapct ~ Proteinan + Fatanimal + 
             agerec + gender + educ + house + bmi1 + totalas + Status.1,
              data = prepmvdata)

model4 <- lm(mmapct ~ VitA + Retinol + 
             agerec + gender + educ + house + bmi1 + totalas + Status.1,
              data = prepmvdata)

model5 <- lm(mmapct ~ VitA + Retinol + Homocyst + 
             agerec + gender + educ + house + bmi1 + totalas + Status.1,
              data = prepmvdata)

model6 <- lm(mmapct ~ VitA + Retinol + Homocyst + Folate + 
             agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model7 <- lm(mmapct ~ VitA + Retinol + Homocyst + Folate + Selen + 
             agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model8 <- lm(mmapct ~ VitA + Retinol + Homocyst + Folate + Selen + Vit.B12 + 
             agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)







model12 <- lm(mmapct ~ Fatanimal + Retinol + Homocyst + Folate + Selen + 
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model13 <- lm(mmapct ~ Fatanimal + Retinol + Homocyst + Folate + Selen + Vit.B12 + 
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model14 <- lm(mmapct ~ Proteinan + Retinol + 
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model15 <- lm(mmapct ~ Proteinan + Retinol + Homocyst + 
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model16 <- lm(mmapct ~ Proteinan + Retinol + Homocyst + Folate + 
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model17 <- lm(mmapct ~ Proteinan + Retinol + Homocyst + Folate + Selen +
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model18 <- lm(mmapct ~ Proteinan + Retinol + Homocyst + Folate + Selen + Vit.B12 +
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model18a <- lm(mmapct ~ Fatanimal + Folate + Selen + 
               agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)


model21uh <- lm(mmapct ~ Vit.B12,  data = prepmvdatahifol)
						
model21um <- lm(mmapct ~ Vit.B12,  data = prepmvdatamidfol)						
						
model21ul <- lm(mmapct ~ Vit.B12,  data = prepmvdatalowfol)	

model21mh <- lm(mmapct ~ Vit.B12 + agerec + gender + educ + house + bmid + totalas + 
						Status.1,  data = prepmvdatahifol)
						
model21mm <- lm(mmapct ~ Vit.B12 + agerec + gender + educ + house + bmid + totalas + 
						Status.1,  data = prepmvdatamidfol)						
						
model21ml <- lm(mmapct ~ Vit.B12 + agerec + gender + educ + house + bmid + totalas + 
						Status.1,  data = prepmvdatalowfol)						

model19 <- lm(dmapct ~ riboflavin + Lycopene + 
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)

model20 <- lm(dmapct ~ riboflavin + Lycopene + LutZea + 
              agerec + gender + educ + house + bmid + totalas + Status.1,
              data = prepmvdata)
             
# wrtie a function to extract all the essential figures from the models

# if the model is x, then...

modelextract <- function(x){
	mycoef <- coef(summary(x))
	iget <- mycoef[-c(1, length(mycoef[,1]): (length(mycoef[,1]) - 13) ), c(1,4) ]
	return(iget)
	}

# models 1 - 3 are inaspct models

inaspct1 <- modelextract(model1)
inaspct2 <- modelextract(model2)
inaspct3 <- modelextract(model3)

inaspcts <- rbind(inaspct1, inaspct2, inaspct3)

write.table(inaspcts, file = "inaspctmultivar.csv", sep = ",")

# models 4-18a are mmapct models

mmamod1 <- modelextract(model4)
mmamod2 <- modelextract(model5)
mmamod3 <- modelextract(model6)
mmamod4 <- modelextract(model7)
mmamod5 <- modelextract(model8)
mmamod6 <- modelextract(model9)
mmamod7 <- modelextract(model10)
mmamod8 <- modelextract(model11)
mmamod9 <- modelextract(model12)
mmamod10 <- modelextract(model13)
mmamod11 <- modelextract(model14)
mmamod12 <- modelextract(model15)
mmamod13 <- modelextract(model16)
mmamod14 <- modelextract(model17)
mmamod15 <- modelextract(model18)
mmamod16 <- modelextract(model18a)

mmapcts <- rbind(mmamod1, mmamod2, mmamod3, mmamod4, mmamod5, mmamod6, 
                 mmamod7, mmamod8, mmamod9, mmamod10, mmamod11, mmamod12,
                 mmamod13, mmamod14, mmamod15, mmamod16)

write.table(mmapcts, file = "mmapctmultivar.csv", sep = ",")

# models 19-20 are dma models

dmamod1 <- modelextract(model19)
dmamod2 <- modelextract(model20)

dmapcts <- rbind(dmamod1, dmamod2)
write.table(dmapcts, file = "dmapctmultivar.csv", sep = ",")

myvariables <- bigdata[, c(5:24, 40:56)]

meansdofvars <- function(x){
	meanvar <- mean(x, na.rm = TRUE)
	sdevar <- sd(x, na.rm = TRUE)/sqrt(length(x))
	meansd <- cbind(meanvar, sdevar)
	return(meansd)
	}
	
meansdofmyvars <- as.data.frame(t(sapply(myvariables, meansdofvars ) ) )
names(meansdofmyvars) = c("mean", "se")

meansdmyvar <- round(meansdofmyvars, 3)

write.table(meansdmyvar, file = "meansdvar.txt", sep = "\t")




 

               





















