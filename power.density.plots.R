## ----------------------- ##
## Computing and Graphing  ##
## Statistical Power       ##
## ----------------------- ##

# Note: This script sources the function shadenorm

## Set Working Directory
# setwd("Z:/Lehre/QuantitativeMethods 2014/Dataproject2014")

## Load the shadenorm.R function file
source("~/Dropbox/Public/R wd/shadenorm.R")

## Parameters of the Problem ##

mu0   <- 0     ## Null-hypothesized value
alpha <- 0.05  ## Significance Level
n1    <- 10000    ## Sample Size
alt   <- 4.5

## -------------------------- ##
## What is Statistical Power? ##
## -------------------------- ##

cuts = c(1-alpha/2, alpha/2)
crits = qnorm(cuts, mu0, 1)    ## Critical Values Based on Null
shadenorm(mu=mu0, sig = 1, outside=crits, col = rgb(0,0,1,1/8)) ## Shadenorm function
shadenorm(mu = 4.5, sig = 1, lines=TRUE, outside=crits, col= rgb(1,0,0,1/8))



