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

############################
### Ex. 1: Meta-analysis ###
############################

### Part a ###
### data frame ###
vac.dis <- c(4,6,3,62,33,180,8,505,29,17,186,5,27)
vac.nodis <- c(119,300,228,13536,5036,1361,2537,87886,7470,1699,50448,2493,16886)
novac.dis <- c(11,29,11,248,47,372,10,499,45,65,141,3,29)
novac.nodis <- c(128,274,209,12619,5761,1079,619,87892,7232,1600,27197,2338,17825)
bcg <- data.frame(vac.dis,vac.nodis,novac.dis, novac.nodis)

bcg
###Part b ###
### n ###
bcg$n.vac <- vac.dis + vac.nodis
bcg$n.novac <- novac.dis + novac.nodis

### p1 and p2 ###
bcg$p2 <- bcg$novac.dis / bcg$n.novac
bcg$p1 <- bcg$vac.dis / bcg$n.vac

### RR ###
bcg$RR <- round(bcg$p1/bcg$p2, 2)

### ln RR ###
bcg$ln.rr <- log(bcg$RR)

### ln SE RR ##
bcg$ln.se.rr <- sqrt(((1-bcg$p1)/bcg$vac.dis)+((1-bcg$p2)/bcg$novac.dis))
head(bcg)

### ln 95% CI RR ###
bcg$low.ln.95ci.rr <- bcg$ln.rr-(1.96*bcg$ln.se.rr)
bcg$up.ln.95ci.rr <- bcg$ln.rr+(1.96*bcg$ln.se.rr)

### 95% CI RR ###
bcg$low.95ci.rr <- round(exp(bcg$low.ln.95ci.rr), 2)
bcg$up.95ci.rr <- round(exp(bcg$up.ln.95ci.rr), 2)
head(bcg)  
bcg

### odds ###
bcg$odds1 <- bcg$p1/(1-bcg$p1)
bcg$odds2 <- bcg$p2/(1-bcg$p2)

### OR ###
bcg$OR <- round(bcg$odds1/bcg$odds2, 2)

### ln OR ###
bcg$lnOR <- log(bcg$OR)

### ln SE OR ###
bcg$ln.se.or <- sqrt((1/bcg$vac.dis)+(1/bcg$vac.nodis) + 
                       (1/bcg$novac.dis)+(1/bcg$novac.nodis))

### ln 95% CI OR ###
bcg$low.ln.95ci.or <- bcg$lnOR - (1.96*bcg$ln.se.or)
bcg$up.ln.95ci.or <- bcg$lnOR + (1.96*bcg$ln.se.or)  

### 95% CI OR ###
bcg$low.95ci.or <- round(exp(bcg$low.ln.95ci.or), 2)
bcg$up.95ci.or <- round(exp(bcg$up.ln.95ci.or), 2)
head(bcg)

### Clean dataset ###
bcg2 <- data.frame(bcg$vac.dis, bcg$vac.nodis, bcg$novac.dis, 
                   bcg$novac.nodis, bcg$RR,bcg$ln.rr, bcg$ln.se.rr, 
                   bcg$low.95ci.rr, bcg$up.95ci.rr, bcg$OR, bcg$lnOR,
                   bcg$ln.se.or)

head(bcg2)

### Part c ###
### fixed effects inverse variance meta-analysis of the odds ratio ###


### Part d ###
### Mantle-Haenszel via  "rmeta" ###
install.packages("rmeta")
library(rmeta)

object.meta.MH  <- meta.MH(ntrt  =	bcg$vac.dis	+	bcg$vac.nodis,	
                           nctrl	=	bcg$novac.dis	+	bcg$novac.nodis,	
                           ptrt	=	bcg$vac.dis,	
                           pctrl	=	bcg$novac.dis,	names	=	1:13)

object.meta.MH
summary(object.meta.MH)
bcg$OR #Compare with previously calculated  ORs

plot(object.meta.MH)
funnelplot(object.meta.MH)

### Part e ###
###  random effects Der-Simonian and Laird meta-analysis ###
object.meta.DSL  <- meta.DSL(ntrt  =  bcg$vac.dis	+	bcg$vac.nodis,	
                             nctrl	=	bcg$novac.dis	+	bcg$novac.nodis,	
                             ptrt	=	bcg$vac.dis,	
                             pctrl	=	bcg$novac.dis,	names	=	1:13)

object.meta.DSL
summary(object.meta.DSL)
bcg$OR #Compare with previously calculated ORs
plot(object.meta.DSL)
par(mfrow = c(1,1))
plot.new()

### Part f ###
### fixed effects Mantle-Haenszel meta-analysis with metafor ###
install.packages("metafor")
library(metafor)

object.rma.mh  <- rma.mh(ai	=	bcg$vac.dis,	bi=	bcg$novac.dis,	ci	
                         =	bcg$vac.nodis,	di	=	bcg$novac.nodis,	data	=	bcg)
object.rma.mh
plot(object.rma.mh)

### Part g ###
### based  on	library	meta ###
install.packages("meta")
library(meta)

object.metabin.mh  <-		metabin(event.e	=	bcg$vac.dis,	
                               n.e	=	bcg$vac.dis	+	bcg$vac.nodis,	
                               event.c =	bcg$novac.dis,	
                               n.c =	bcg$novac.dis	+	bcg$novac.nodis,	
                               method="MH")
object.metabin.mh

forest.meta(object.metabin.mh)
funnel(object.metabin.mh)

#  Inverse	Variance:
object.metabin.iv	<-		metabin(event.e	=	bcg$vac.dis,	
                              n.e	=	bcg$vac.dis  +	bcg$vac.nodis,	
                              event.c	=	bcg$novac.dis,	
                              n.c =	bcg$novac.dis  +	bcg$novac.nodis,	
                              method="Inverse")
object.metabin.iv
bcg$RR #Compare with previously calculated  RRs
forest.meta(object.metabin.iv)

#------------------------------------------------------------------------------------#

#############################################
### Ex. 2:  Fixed effects meta-regression ###
#############################################

### Part i ###
### add latitude ###
bcg$lat <- c(44,55,42,52,13,44,19,13,-27,42,18,33,33)

### Part j ###
### add weight ###
bcg$w <- 1/bcg$ln.se.rr^2

### Latitude	(centered) ###
mn.dist.eq	<- mean(abs(bcg$lat))
bcg$lat.cent	<- (abs(bcg$lat)	- mn.dist.eq)

### Part k ###
### Regression Model ###
lm.bcg  <- lm(bcg$ln.rr	~	bcg$lat.cent,	weight	=	bcg$w,	data	=	bcg)
summary(lm.bcg)
anova(lm.bcg)

### Part l ###
### Extract MSE from anova and correct standard errors of parameters ###
index.tmp  <- which(attributes(anova(lm.bcg))$row.names=="Residuals") ### UNNECESARY 
MSE.bcg	<- anova(lm.bcg)$"Mean Sq"[index.tmp] ### SEE SIMPLER WAY BELOW

MSE.bcg  <- anova(lm.bcg)$"Mean Sq"[2]
MSE.bcg

#  Corrected	standard	error	of	the	intercept:
0.074456/sqrt(MSE.bcg) ### See below alternative way

# First way
se.lm.bcg <- sqrt(diag(vcov(lm.bcg))) # Extract/Calculate standard errors
se.intercept.lm.bcg <- se.lm.bcg[1] # Store each standard error on an object: intercept
se.lat.cent.lm.bcg <- se.lm.bcg[2] # Store each standard error on an object: centered latitude

# Second way
se.lm.bcg <- summary(lm.bcg)$coefficients["(Intercept)","Std. Error"]
se.intercept.lm.bcg <- summary(lm.bcg)$coefficients["bcg$lat.cent","Std. Error"]

# Alternative: Corrected  standard	error	of	the	intercept:
se.intercept.lm.bcg/sqrt(MSE.bcg)





