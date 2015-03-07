setwd("~/Documents")
mydata <- read.csv("madata.csv", header = TRUE)
print(names(mydata))

# We are going to write functions on MA based on Normand
# Check with Normand studies
# define risk difference
# riskdiff <- p.t - p.c # p.t = outcome.t/(n.c + n.t)
# p.c = outcome.c/(n.c + n.t)
# For binary outcomes
# first we compute the risk differences
riskdiff <- function(n.t, n.c, o.t, o.c){
  p.t <- o.t/n.t
  p.c <- o.c/n.c
  r.diff <- round((p.t - p.c)*100,2)
  return(r.diff)
}

# We compute the sd for the risk differences
# This can give us the upper and lower 95% confidence intervals
# n.t = numbers treated, n.c = numbers control, 
# o.t = outcomes in treated, o.c = outcomes in controls
serd <- function(n.t, n.c, o.t, o.c){
  p.t <- (o.t/n.t)*100
  q.t <- (100 - p.t)
  p.c <- (o.c/n.c)*100
  q.c <- (100 - p.c)
  add2 <- ((p.t * q.t/n.t) + (p.c * q.c/n.c))
  se <- sqrt(add2)
  return(se)
}

my.se1 <- serd(mydata$lido.n, 
               mydata$ctrl.n,
               mydata$dead.lido,
               mydata$dead.ctrl)

rdiff.u <- 

#print(riskdiff(39,43,2,1))

 rd <- riskdiff(mydata$lido.n, mydata$ctrl.n,
      mydata$dead.lido, mydata$dead.ctrl)

rd.u <- round(rd + (1.96 * my.se1), 2)
rd.l <- round(rd - (1.96 * my.se1), 2)

rdlist <- cbind(rd, rd.l, rd.u)

# Next, we are going to study relative risks 
# relative risk will be ratio of risk among exposed or intervention
# and risk among controls

relrisk <- function(ntx, ncx, o.tx, o.cx){
  ptx <- (o.tx/ntx)*100
  qtx <- 100 - ptx
  pcx <- (o.cx/ncx)*100
  qcx <- 100 - pcx
  rr <- round(ptx/pcx, 2)
  sum1 <- (qtx/(ntx*ptx)) + (qcx/(ncx * pcx ))
  se.logrr <- sqrt(sum1)
  rr.l <- round(exp(log(rr) - (1.96 * se.logrr)), 2)
  rr.u <- round(exp(log(rr) + (1.96 * se.logrr)), 2)
  return(cbind(rr, rr.l, rr.u))
}

# print(relrisk(mydata$lido.n, mydata$ctrl.n,
 #             mydata$dead.lido, mydata$dead.ctrl))
# Next, we are going to observe Odds Ratios for binary

oddsrat <- function(ntr, ncr, otr, ocr){
  ptr <- otr/ntr
  qtr <- 1 - ptr
  pcr <- ocr/ncr
  qcr <- 1 - pcr
  oddsr <- round( (ptr * qcr)/(qtr * pcr), 2)
  se.logor <- sqrt(1/otr + 1/(ntr - otr) + 1/ocr + 1/(ncr - ocr))
  oddsr.l <- round(exp(log(oddsr) - (1.96 * se.logor)), 2)
  oddsr.u <- round(exp(log(oddsr) + (1.96 * se.logor)), 2)
  return(cbind(oddsr, oddsr.l, oddsr.u))
}

#print(oddsrat(mydata$lido.n, mydata$ctrl.n,
     #         mydata$dead.lido, mydata$dead.ctrl))