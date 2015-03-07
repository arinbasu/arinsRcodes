setwd("C:/Users/arinbasu/Documents/Projects/ArsenicProjectwithAllan/workon090909_eod")
require(foreign)
masterdata <- read.spss("newWrkspsav.sav", to.data.frame = TRUE)
write.csv(masterdata, "masterdata.csv")
bigdata <- read.csv("bigdata2a.csv", sep = ",")
bigdata2 <- cbind(bigdata, bmi = masterdata$BMI )
workdata0 <- read.csv("workable.csv", sep = ",")
workdata1 <- cbind(workdata, bmi1 = bigdata2$bmi)
workdata <- workdata1[, c(2:length(workdata1))]
cleandata <- read.csv("cleandata2a.csv", na.strings = "NA")
# select only those individuals who have complete records for
# diet, micronutrients, and methylation variables
# function to create tertiles and anova based p values for the tertiles
# cut bmi1 into three parts < 16.9, 16.9-19.3, and > 19.3
# create a variable with three levels of body mass index

names(cleandata)
length(cleandata$SUBJECT)
table(cleandata$STATUS)
length(cleandata$TOTAS)
summary(cleandata$TOTAS)
#tertiles of total arsenic
t.totas <- quantile(cleandata$TOTAS, probs = seq(0,1,0.33), na.rm = T)
cleandata$ttotas <- cut(cleandata$TOTAS, breaks = c(-1, t.totas,max(cleandata$TOTAS, na.rm = T)))
table(cleandata$ttotas)

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

dataset1 <- bigdata2[, c(5:21,23:24, 39:45, 47:56)]

# develop the corresponding dataset
# first create the inorganic arsenic, mma, dma percentages, thus:
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
# so the following dataset is a  composite dataset containing all of them

dietmunutscaladj <- data.frame(cbind(cleandata[,c(1:7,15)], cleandatacal, cleandata[,c(43:76)]))
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

bmifunction <- function(x = workdata$bmid,y){
	bmif1 <- cbind(	mean(y[x == "low"], na.rm = TRUE),
	                mean(y[x == "mid"], na.rm = TRUE),
	                mean(y[x == "high"], na.rm = TRUE))
	pvalue <- anova(lm(y ~ x))$"Pr(>F)"[1]
	finaloutbmi <- cbind(bmif1, pvalue)
	return(finaloutbmi)
	}
	
	                
	

inastotalas <- tertotas(workdata$totalas, workdata$inaspct)
mmatotalas <- tertotas(workdata$totalas, workdata$mmapct)
dmatotalas <- tertotas(workdata$totalas, workdata$dmapct)     

workdata$totaltertiles <- cut(workdata$totalas, 
                              breaks = quantile(workdata$totalas, na.rm = TRUE, 
                              probs = seq(0,1,0.3) ) ) 




# the following is a good function but I am going to write a better one below
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

# the following function is a better one because it takes into account overlaps

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

inasvalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], goodtert2, cleandata$inpct)))
inasval <- inasvalues[ order(-abs(inasvalues$X3)), ]

mmavalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], goodtert2, cleandata$mmapct)))
mmaval <- mmavalues[ order(-abs(mmavalues$X3)), ]

dmavalues = data.frame(t(sapply(dietmunutscaladj[, c(9:47)], goodtert2, cleandata$dmapct)))
dmaval <- dmavalues[ order(-abs(dmavalues$X3)), ]



# these are the old estimates, may be deprecated              
# ttestinas <- data.frame(t(sapply(workdata[,c( 3:34, 36:53)], tertest, workdata$inaspct)))
# ttestinas1 <- ttestinas[ order(-abs(ttestinas$X3)), ]
# ttestmma <- data.frame(t(sapply(workdata[,c(3:34, 36:53)], tertest, workdata$mmapct)))
#ttestmma1 <- ttestmma[ order(-abs(ttestmma$X3)), ] 

# ttestdma <- data.frame(t(sapply(workdata[,c(3:34, 36:53)], tertest, workdata$dmapct)))
ttestdma1 <- ttestdma[ order(-abs(ttestdma$X3)), ]

# function to create tertiles and cross tabulate x and y

tertilecompare <- function(x,y){
	tertx <- cut(x, breaks = quantile(x, na.rm = TRUE, probs = seq(0,1,0.3)), 
	             labels = c("qx1","qx2","qx3") )
	terty <- cut(y, breaks = quantile(y, na.rm = TRUE, probs = seq(0,1,0.3)), 
	             labels = c("qy1","qy2","qy3") )
	twocor <- cor(tertx, terty, use = "all.obs")
	return(summary(twocor))
	}

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
	tertilex1 <- cut(x, breaks = quantile(x, na.rm = T, probs = seq(0,1,0.3)))
	}
	
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




 

               





















