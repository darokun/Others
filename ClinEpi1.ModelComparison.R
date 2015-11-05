#########################
### Preliminary stuff ###
#########################

# When working from home
#setwd("~/Dropbox/Public/R wd")

# When working at KR5
#setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")

#------------------------------------------------------------------------------------#

#################
### Execution ###
#################

########################
### Model Comparison ###
########################

### To build the models:
F <- c(10,11,13,9,7,16,50,16,4,18,60,27,5,21,125,91)
K <- c(38,26,36,8,27,35,60,19,12,16,49,14,8,20,52,27)
alkohol <- c(0:3)%x%rep(1,4)
tabak <- rep(c(0:3),4)
alkoholf <- as.factor(alkohol)
tabakf <- as.factor(tabak)
alkoholo <- ordered(alkohol)
tabako <- ordered(tabak)
alkoholm<-rep(c(0.0, 0.2, 1.0, 2.0),rep(4,4))
tabakm<-rep(c(0, 10, 30, 50),4)
no.smoker<-ifelse(tabak==0,1,0)

### Summary of the models:
m1 <- glm(cbind(F,K) ~ alkoholf + tabakf, family = binomial)
m2 <- glm(cbind(F,K) ~ alkoholf * tabakf, family = binomial)
m3 <- glm(cbind(F,K) ~ alkoholf, family = binomial)
m4 <- glm(cbind(F,K) ~ tabakf, family = binomial)
m5 <- glm(cbind(F, K)~alkoholo+tabako, family = binomial)
m6 <- glm(cbind(F, K)~alkohol+tabak, family = binomial)
m7 <- glm(cbind(F, K)~alkoholm+tabakm, family = binomial)
m8 <- glm(formula = cbind(F, K) ~ alkohol + I(alkohol^2) +
            tabak + I(tabak^2), family = binomial)
m9<-glm(cbind(F, K) ~ alkohol + no.smoker + tabak, family = binomial)

# Model table - Slide 29
model.type <- c("Independent - nominal", "Interaction - nominal",
                "Only tobacco - nominal", "Only alcohol - nominal", 
                "Independent - ordinal", "Independent - 0,1,2,3 coded - linear",
                "Independent - mean coded - linear", 
                "Independent - 0,1,2,3 coded - non-linear",
                "Independent - 0,1,2,3 coded - linear - non-smoker")
model <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9" )
aic.model<-c(86.61,97.92,135.8,111.9,86.61,79.72,81.66,82.83,81.44)
aic.delta<-aic.model-min(aic.model)
aic.weights<-exp(-0.5*aic.delta)/sum(exp(-0.5*aic.delta))

cbind(model.type,model,aic.model,aic.delta,round(aic.weights,4)) # Table on slide 29

# Strengths of evidence - Slide 31
mm<-outer(aic.weights,aic.weights,function(x,y){x/y})
dimnames(mm)<-list(paste("M",1:9,sep="."),paste("M",1:9,sep="."))
round(mm,0)

#####################
### END OF SCRIPT ###
#####################