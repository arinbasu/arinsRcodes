# require(sem)
# help(sem)
require(OpenMx)
require(MASS)
require(psych)
# source('http://openmx.psyc.virginia.edu/getOpenMx.R')

# one factor model

data(demoOneFactor)
manifests <- names(demoOneFactor) #names of variables in one factor dataset
latents <- c("G") #just one thing called "G"

factorModel <- mxModel(name="One Factor",
                       type = "RAM",
                       manifestVars = manifests,
                       latentVars = latents,
                       mxPath(from=latents, to=manifests),
                       mxPath(from=manifests, arrows=2),
                       mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
                       mxData(observed=cov(demoOneFactor), type="cov", numObs=500)
                       )



factorFit <- mxRun(factorModel)
summary(factorFit)

## Now run with matrix model specification

factormod2 <- mxModel(name = "One Factor As Before",
                      mxMatrix(type="Full", nrow=5,
                               ncol=1, free=T, 
                               values=0.2, 
                               name="A"),
                      mxMatrix(type="Symm", nrow=1, ncol=1,
                               free=FALSE, values=1,
                               name="L"),
                      mxMatrix(type="Diag", nrow=5, ncol=5,
                               free=T, values=1,
                               name="U"),
                      mxAlgebra(expression=A %*% L %*% t(A) + U, 
                                name="R"),
                      mxMLObjective(covariance="R", 
                                    dimnames = names(demoOneFactor)),
                      mxData(observed=cov(demoOneFactor), type="cov",
                             numObs=500)
                      )

factorFit2 <- mxRun(factormod2)
summary(factorFit2)

## the following are some matrix examples

examples <- mxModel("declare matrices",
                    mxMatrix(
                      type="Full",
                      nrow=3,
                      ncol=1,
                      values=c(1,2,3),
                      name="A"),
                    
                    mxMatrix(
                      type="Full",
                      nrow=3,
                      ncol=1,
                      values=c(1,2,3),
                      name="B")
                    )

## q1 = A + B
additionof2matx <- mxAlgebra(
  expression = A + B,
  name = "q1")

## q2 = A * A (multiplication of two matrices)

multiplicationof2matx <- mxAlgebra(
  expression = A * B,
  name = "q2")

## q3 = t(A)

transposeofA <- mxAlgebra(
  expression = t(A),
  name="q3")

## q4 = A * transpose of A

atimesta <- mxAlgebra(
  expression = A %*% t(A),
  name = "q4")

## run matrix algebra exercises

algebraex <- mxModel("algebra exercises",
                     mxMatrix(type="Full", nrow=3, ncol=1,
                              values = c(1,2,3), name = "A"),
                     mxMatrix(type="Full", nrow=3, ncol=1,
                              values=c(1,2,3), name = "B"),
                     mxAlgebra(expression = A + B, name = "q1"),
                     mxAlgebra(expression = A * A, name = "q2"),
                     mxAlgebra(expression = t(A), name = "q3"),
                     mxAlgebra(expression = A %*% t(A), name = "q4"),
                     mxAlgebra(expression = t(A) %*% A, name = "q5")
                       
                      )
answrs <- mxRun(algebraex)
results <- mxEval(list(q1, q2, q3, q4, q5), answrs)

## Now, we are going to run some simulations where 
## two variables will be correlated 0.5 with 1000 individuals,
## and they each will have mean = 0, and variance = 1

set.seed(200)
rs = 0.5
## the following is where the simulation occurs
xy <- mvrnorm(1000, c(0,0),
              matrix(c(1, rs, rs, 1), 2, 2)
)
testData <- xy
selVars <- c("X", "Y")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
cov(testData)

## Now let's see if these data X and Y are correlated
## We have two means, two variances, and one covariance
## first one is expected mean matrix both means are 0, 0

bivCorModel <- mxModel("bivCor",
              mxMatrix(
                type = "Full",
                nrow = 1,
                ncol = 2,
                free = TRUE,
                values = c(0,0),
                name = "ExpMean"),
                       
             mxMatrix(
               type = "Lower",
               nrow = 2,
               ncol = 2,
               free = TRUE,
               values = 0.5,
               name = "Chol"),
            
            mxAlgebra(
              expression = Chol %*% t(Chol),
              name = "ExpectedCovariance"),
                       
            mxData(
              observed = testData,
              type = "raw"),
                       
            mxFIMLObjective(
              covariance = "ExpectedCovariance",
              means = "ExpMean",
              dimnames = selVars)           
                       
                       )

bivCorFit <- mxRun(bivCorModel)

## Now, let's test if the covariance is significantly 
## different from zero

bivCorModelSub <- mxModel(bivCorModel,
                  mxMatrix(
                    type = "Diag",
                    nrow = 2,
                    ncol = 2,
                    free = TRUE,
                    name = "Chol")
                        )

bivCorFitSub <- mxRun(bivCorModelSub)

ems <- mxEval(ExpMean, bivCorFitSub)
ecs <- mxEval(ExpectedCovariance, bivCorFitSub)
lls <- mxEval(objective, bivCorFitSub)
Chi = lls - ll;
lrt = rbind(ll, lls, Chi); lrt

## At this stage we are going to simulate
## twin studies
## We are testing ACE model, where 
## A = Additive genetic component, C = shared environmental 
## component, and E = error and non-shared environmental component
## for mz twins, all genes are identical, for dz twins, only
## 50% genes are shared and some shared environment
## thus a for dz is 0.5 * a for MZ
## Here, we assume that A accounts for 50% variance, 
## C accounts for 30% variance, and E accounts for 20% 
## variance for some traits
## if this is true, for mz, a^2 + c^2 = 0.8 (0.5 + 0.3) and
## for dz, a^2 + c^2 = 0.25 + 0.30 = 0.55

## So, we start the simulation

set.seed(200)
a2 <- 0.5
c2 <- 0.3
e2 <- 0.2
## rmz = correlation for mz twins, rdz = dizygotic twins
rmz <- a2 + c2
rdz <- 0.5 * a2 + c2
datamz <- mvrnorm(1000, c(0,0), matrix(c(1, rmz, rmz, 1), 2,2))
datadz <- mvrnorm(1000, c(0,0), matrix(c(1, rdz, rdz, 1), 2,2))

## t1 and t2 represent the pairs of the twins
## so, t1 is say the index twin, and t2 is the co-twin

selectedvars <- c("t1", "t2")

colnames(datamz) <- selectedvars
colnames(datadz) <- selectedvars

## so, for the mz twins, see

mxModel("MZ",
        mxMatrix(
          type = "Full",
          nrow = 1,
          ncol = 2,
          free = TRUE,
          values = c(0,0),
          name = "expmeanmz"),
        mxMatrix(
          type = "Lower",
          nrow = 2,
          ncol = 2,
          free = TRUE,
          values = 0.5,
          name = "cholmz"),
        mxAlgebra(
          expression = cholmz %*% t(cholmz),
          name = "expectedcovmz"),
        mxData(
          observed = datamz,
          type = "raw"),
        mxFIMLObjective(
          covariance = "expectedcovmz",
          means = "expmeanmz",
          dimnames = selectedvars)
        )

## Now, very similar for dz twins,

mxModel("DZ",
        mxMatrix(
          type = "Full",
          nrow = 1,
          ncol = 2,
          free = TRUE,
          values = c(0,0),
          name = "expmeandz"),
        mxMatrix(
          type = "Lower",
          nrow = 2,
          ncol = 2,
          free = TRUE,
          values = 0.5,
          name = "choldz"),
        mxAlgebra(
          expression = choldz %*% t(choldz),
          name = "expectedcovdz"),
        mxData(
          observed = datadz,
          type = "raw"),
        mxFIMLObjective(
          covariance = "expectedcovdz",
          means = "expmeandz",
          dimnames = selectedvars)
)

## Now, combine the two above models to a supermodel

twinsatmodel <- mxModel("twinsat",
                        mxModel("MZ",
                                mxMatrix(
                                  type = "Full",
                                  nrow = 1,
                                  ncol = 2,
                                  free = TRUE,
                                  values = c(0,0),
                                  name = "expmeanmz"),
                                mxMatrix(
                                  type = "Lower",
                                  nrow = 2,
                                  ncol = 2,
                                  free = TRUE,
                                  values = 0.5,
                                  name = "cholmz"),
                                mxAlgebra(
                                  expression = cholmz %*% t(cholmz),
                                  name = "expectedcovmz"),
                                mxData(
                                  observed = datamz,
                                  type = "raw"),
                                mxFIMLObjective(
                                  covariance = "expectedcovmz",
                                  means = "expmeanmz",
                                  dimnames = selectedvars)
                        ),
                        mxModel("DZ",
                                mxMatrix(
                                  type = "Full",
                                  nrow = 1,
                                  ncol = 2,
                                  free = TRUE,
                                  values = c(0,0),
                                  name = "expmeandz"),
                                mxMatrix(
                                  type = "Lower",
                                  nrow = 2,
                                  ncol = 2,
                                  free = TRUE,
                                  values = 0.5,
                                  name = "choldz"),
                                mxAlgebra(
                                  expression = choldz %*% t(choldz),
                                  name = "expectedcovdz"),
                                mxData(
                                  observed = datadz,
                                  type = "raw"),
                                mxFIMLObjective(
                                  covariance = "expectedcovdz",
                                  means = "expmeandz",
                                  dimnames = selectedvars)
                        ),
                        mxAlgebra(MZ.objective + DZ.objective,
                                  name = "minus2ll"),
                        mxAlgebraObjective("minus2ll")
                        
                  )

twinsatfit <- mxRun(twinsatmodel)

expmeanmz <- mxEval(MZ.expmeanmz, twinsatfit); print(expmeanmz)
expcovmz <- mxEval(MZ.expectedcovmz, twinsatfit); print(expcovmz)
expmeandz <- mxEval(DZ.expmeandz, twinsatfit); print(expmeandz)
expcovdz <- mxEval(DZ.expectedcovdz, twinsatfit); print(expcovdz)
ll.sat <- mxEval(objective, twinsatfit); print(ll.sat)

twinsatmodelsub1 <- mxModel(twinsatmodel, name="twinsatsub1")
twinsatmodelsub1$MZ$expmeanmz <- mxMatrix("Full", 1,2,TRUE, 0, "mMZ")
twinsatmodelsub1$MZ$expmeanmz <- mxMatrix("Full", 1, 2, T, 0, "mDZ")
twinsatfitsub1 <- mxRun(twinsatmodelsub1)

ll.sub1 <- mxEval(objective, twinsatfitsub1); print(ll.sub1)
print(summary(twinsatfit))

## We are going to specify the ACE model
## at this stage (3rd dec, I do not understand it fully)

twinacemodel <- mxModel("twinACE",
                    mxModel("ACE",
                        mxMatrix(type="Full", nrow = 1, 
                                 ncol = 2, free = T,
                                 values = 20,
                                 label = "mean",
                                 name = "expmean"),
                        mxMatrix(type = "Full", nrow = 1,
                                 ncol = 1, free = T,
                                 values = 0.6,
                                 label = "a11",
                                 name = "a"),   
                        mxMatrix(type = "Full", nrow = 1,
                                     ncol = 1, free = T,
                                     values = 0.6,
                                     label = "c11",
                                     name = "c"), 
                       mxMatrix(type = "Full", nrow = 1,
                                     ncol = 1, free = T,
                                     values = 0.6,
                                     label = "e11",
                                     name = "e"),     
                       
                      mxAlgebra( expression = a * t(a), name = "A"),
                      mxAlgebra( expression = c * t(c), name = "C"),
                      mxAlgebra( expression = e * t(e), name = "E"),
                     # matrixes expected cov matrix for mz twins
                      mxAlgebra( expression = rbind( 
                        cbind(A+C+E, A+C),
                        cbind(A+C, A+C+E)),
                                 name = "expectedcovmz"),
                            
                      mxAlgebra( expression = rbind( 
                        cbind(A+C+E, 0.5%x%A+C),
                        cbind(0.5%x%A+C, A+C+E)),
                                  name = "expectedcovdz")      
                    ),
            mxModel( "MZ",
              mxData( observed = datamz, type = "raw"),
              mxFIMLObjective( covariance = "ACE.expectedcovmz",
                               means = "ACE.expmean",
                               dimnames = selectedvars)       
                     ),
            mxModel( "DZ", 
                     mxData( observed = datadz, type = "raw"),
                     mxFIMLObjective( covariance = "ACE.expectedcovdz",
                                      means = "ACE.expmean",
                                      dimnames = selectedvars)      
                     ),
            mxAlgebra(MZ.objective + DZ.objective, 
                      name = "minus2LL"),
            mxAlgebraObjective("minus2LL")            
            )

twinacefit <- mxRun(twinacemodel)

ll.ace <- mxEval(objective, twinacefit); print(ll.ace)
lrt.ace <- ll.ace - ll.sat; print(lrt.ace)

A <- mxEval(ACE.A, twinacefit)
C <- mxEval(ACE.C, twinacefit)
E <- mxEval(ACE.E, twinacefit)

V <- (A+C+E)
a2 <- A/V
c2 <- C/V
e2 <- E/V

print(c(a2, c2, e2))

## Use a path model

## simulate data

set.seed(100)
x <- rnorm(1000, 0, 1)
testdata <- as.matrix(x)
selectedvars <- c("X")
dimnames(testdata) <- list(NULL, selectedvars)
summary(testdata)
var(testdata)

univsatmodel <- mxModel("univsat1",
                        type = "RAM",
                        manifestVars = selectedvars,
                        mxPath(
                          from = c("X"),
                          arrows = 2,
                          free = T,
                          values = 1,
                          lbound = 0.01,
                          labels = "vX"),
                        mxData(
                          observed = var(testdata),
                          type = "cov",
                          numObs = 1000)
                        )
