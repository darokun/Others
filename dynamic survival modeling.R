#########################
### Preliminary stuff ###
#########################

# When working from home
# setwd("~/Dropbox/Public/R wd")

# When working at KR5
setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")

#------------------------------------------------------------------------------------#

#################
### Execution ###
#################

#############################
### Dynamic Cox modelling ###
#############################

# Install and load required packages:
require("survival")

# Read data
uis <- read.csv("~/Dropbox/Public/R wd/uis.csv")
head(uis)

# Plot: Survival function versus survival time
plot(survfit(formula = Surv(time, censor)~ treat, data = uis, conf.type="none"), 
     lty=1:2, xlab="Time", ylab="Survival Probability" )

# Tests and Graps Based on the Schoenfeld Residuals 
time.dep <- coxph( Surv(time, censor)~age+race+treat+ site+age:site,
                   uis, method="breslow", na.action=na.exclude)
time.dep.zph <- cox.zph(time.dep, transform = 'log')
time.dep.zph
plot(time.dep.zph)

#plots for the predictors: age and treat including the reference line at y=0 
plot(time.dep.zph[1]) # almost linear
abline(h=0, lty=3) 

plot(time.dep.zph[2]) # sinusoidal?
abline(h=0, lty=3)

plot(time.dep.zph[3]) # u shaped
abline(h=0, lty=3)

plot(time.dep.zph[4]) # almost linear
abline(h=0, lty=3)





