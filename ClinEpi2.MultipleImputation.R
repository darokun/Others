#########################
### Preliminary stuff ###
#########################

# When working from home
#setwd("~/Dropbox/Public/R wd")

# When working at KR5
#setwd("/usr281/ben/msc1426/Desktop/Missing Data")

#------------------------------------------------------------------------------------#

#################
### Execution ###
#################

###########################
### Multiple imputation ###
###########################

### SIMULATION DATA

install.packages("Amelia")
library(Amelia)
install.packages("Zelig")
library(Zelig)
install.packages("tcltk")
library(tcltk)

# simulate data
n <- 500
p <- 8
set.seed(13)
x <- matrix(rnorm(p*n), nrow=n)

rho <- 0.5
sigma <- 1

H <- abs(outer(1:p, 1:p, "-"))
V <- sigma * rho^H


simdat <- read.csv("data.m.10.csv", header = T)
class(simdat)
head(simdat)
cor(simdat, use = 'complete.obs')
c.model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = simdat)
summary(c.model)
str(simdat)

data.imputed <- amelia(simdat, m = 5, idvars = "id")
data.imputed

plot(data.imputed)

summary(zelig(Y ~ X1+X2+X3+X4+X5+X6+X7+X8,
              model="ls", data = data.imputed$imputations))





### NHANES DATA
tab <- read.csv("nhanesdata_v11a2.csv", header = T)
str(tab)
summary(tab)

### data clean
tab$educ <- as.factor(tab$educ)
tab$ethnic <- as.factor(tab$ethnic)
tab$age_cat <- as.factor(tab$age_cat)
tab$meat_cat <- as.factor(tab$meat_cat)

# complete case analysis
model1 <- lm(rr_sys ~ bmicenter + age_cat + male2, data = tab)
summary(model1)

model2 <- lm(rr_sys ~ bmicenter + age_cat + male2 + meat_prvmo2, data = tab)
summary(model2)

# meat problem
plot(density(tab$meat_prvmo2, na.rm = T))
summary(tab$meat_prvmo2, na.rm=T)

plot(tab$meat_cat) # 12,28,56,>56

plot(density(log(tab$meat_prvmo2), na.rm = T))

# model w cat meat
model3 <- lm(rr_sys ~ bmicenter + age_cat + male2 + meat_cat, data = tab)
summary(model3)

# work with nas

AmeliaView() # doesn't work on this R version

missmap(tab)
set.seed(2)

Delete <- c(1,5,6,10)
tab2 <- tab[,-Delete]

set.seed(2)
tab2.mi1 <- amelia(tab2, m=5, idvars = 'seqn', noms=c('age_cat', 'male2'))
summary(tab2.mi1)

tab2.mi2 <- amelia(tab2,m=5,idvars = 'seqn', noms=c('age_cat', 'male2'), logs = "meat_prvmo2")
summary(tab2.mi2)

Delete2 <- c(1,5,6,9)
tab3 <- tab[,-Delete2]

tab3.mi3 <- amelia(tab3,m=5,idvars = 'seqn', noms=c('age_cat', 'male2', 'meat_cat'))
summary(tab3.mi3)

tab.mi4 <- amelia(tab,m=5,idvars = 'seqn', noms=c('age_cat', 'male2', 'meat_cat', 'educ', 'ethnic'))
summary(tab.mi4)


#analyze with zelig
set.seed(2)
summary(zelig(rr_sys ~ bmicenter + age_cat + male2 + meat_prvmo2, model="ls", data = tab2.mi1$imputations))

names(tab2.mi1$imputations)
summary(tab2.mi1$imputations$imp1)

summary(zelig(rr_sys ~ bmicenter + age_cat + male2 + meat_prvmo2, data = tab2.mi1$imputations, model = 'ls'))

#diagnosis plots

plot(tab2.mi1)
plot(tab2.mi2)
plot(tab3.mi3)
plot(tab.mi4, overimpute = T)


# imputed values (red distribution) have less variability because they are computed from other conditional and correlated variables

plot.new()
hist(tab$bmicenter)
par(mfrow=c(1,1))
names()
dir()

child.dat <- read.table("Childnutrition_Exampledata.txt", sep ="\t", header = T)
child.dat$numberdays <- as.factor(child.dat$numberdays)
missmap(child.dat)

summary(child.dat)
ch.dat1 <- child.dat[,c(3,4,8,9,10,11)]

ch.im1 <- amelia(ch.dat1, m=5, ords=c('numberdays'))

# do the rest of the analysis on these data
