#########################
### Preliminary stuff ###
#########################

# When working from home
setwd("~/Dropbox/Public/R wd")

# When working at KR5
setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")

#------------------------------------------------------------------------------------#

#################
### Execution ###
#################

########################################
### Exploratory Factor Analysis in R ###
########################################

install.packages("psych")
library(psych)

data(bfi)
describe(bfi)

# parallel analysis on these data to see if a 5 
# dimensional solution is reasonable
fa.parallel(bfi)


# 5 factor, orthogonal solution. 
# Using the base package function for maximum likelihood factor analysis:
m15.out <- factanal(covmat = cor(bfi, use = "complete.obs"), 
                    factors = 5, rotation = "none") #Max Likelihood FA
m15.out

# Notice that, by default, R fails to print loadings lower than an 
# arbitrary threshold. To see all loadings:
print(loadings(m15.out), cutoff = 1e-05) 

# Principal axes factor analysis with varimax rotation:
pa.out <- factor.pa(r = bfi, nfactors = 5, residuals = FALSE,
                    rotate = "varimax", n.obs = NA, scores = FALSE, SMC = TRUE,
                    missing = FALSE, impute = "median", min.err = 0.001, digits = 2,
                    max.iter = 100, symmetric = TRUE, warnings = TRUE, fm = "pa")
pa.out

# to see all loadings
print(pa.out$loadings, cutoff = 1e-05, digits = 2)

# The problem of local minima: Varimax
install.packages("GPArotation")
library(GPArotation)
pa5.loadings <- print(loadings(pa.out), cutoff = 1e-08)

# No idea what this function does
Global.min <- function(A, method, B = 10) {
  fv <- rep(0, B)
  seeds <- sample(1e+07, B)
  for (i in 1:B) {
    cat(i, " ")
    set.seed(seeds[i])
    gpout <- GPForth(A = A, Random.Start(ncol(A)), method = method)
    dtab <- dim(gpout$Table)
    fv[i] <- gpout$Table[dtab[1], 2]
    cat(fv[i], "\n")
  }
  cat("Min is ", min(fv), "\n")
  set.seed(seeds[order(fv)[1]])
  ans <- GPForth(A = A, Random.Start(ncol(A)), method = method,
                 normalize = TRUE)
  ans
}

Global.min(pa5.loadings, "varimax", 10)


# The problem of local minima: Oblique Rotation:
Global.min <- function(A, method, B = 10) {
  fv <- rep(0, B)
  seeds <- sample(1e+07, B)
  for (i in 1:B) {
    cat(i, " ")
    set.seed(seeds[i])
    gpout <- GPFoblq(A = A, Random.Start(ncol(A)), method = method)
    dtab <- dim(gpout$Table)
    fv[i] <- gpout$Table[dtab[1], 2]
    cat(fv[i], "\n")
  }
  cat("Min is ", min(fv), "\n")
  set.seed(seeds[order(fv)[1]])
  ans <- GPFoblq(A = A, Random.Start(ncol(A)), method = method)
  ans
}

Global.min(pa5.loadings, "oblimin", 10)

