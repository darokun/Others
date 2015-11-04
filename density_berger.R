
nhanesv11 <- read.delim('~/Dropbox/Public/R wd/dataproject_datasets/nhanesdataset_a.tsv', header= T)
dim(nhanesv11)
names(nhanesv11)
summary(nhanesv11)
nhanesv11$male
nhanesv11$sex <- as.numeric(nhanesv11$male)
sum(nhanesv11$sex)
nhanesv11fem <- nhanesv11[nhanesv11$sex==0,]
dim(nhanesv11fem)
?ecdf

hist(nhanesv11fem$height,
     freq=FALSE, breaks=50, 
     main="Histogram: \n Height of Women (NHANES Study)", 
     xlab="height (cm)", xaxp=c(120,200,16))

plot(ecdf(nhanesv11fem$height), 
     verticals = TRUE, do.points = FALSE, 
     main = "Cum. Distribution: \n Height of Women (NHANES Study)", 
     xlab = "height (cm)",  yaxp=c(0,1,20), xaxp=c(120,200,16))

abline(h=seq(0,1,by=0.05))

boxplot(nhanesv11fem$height, main="Boxplot: \n Height of Women (NHANES Study)",
        ylab="height (cm)")

abline(h=seq(120,200,by=10))


mean(nhanesv11fem$height,na.rm=TRUE)
sd(nhanesv11fem$height,na.rm=TRUE)
quantile(nhanesv11fem$height,na.rm=TRUE, probs=c(0,0.25, 0.5, 0.75, 0.95, 1))


#******
par(bty="l")

plot(density(nhanesv11fem$height, na.rm=TRUE, bw="SJ", adjust=2),  
     bty = "l", col="springgreen4", lwd=5, xlab="x", 
     ylab="density f(x)", cex.lab=1.2, cex.axis=1.2, main="")
lines(density(nhanesv11fem$height, na.rm=TRUE, bw="SJ", adjust=2), lwd=2)

plot(ecdf(nhanesv11fem$height), verticals = TRUE, 
     do.points = FALSE,  bty = "l",col="springgreen4",
     xlab = "x", ylab="F(x)", lwd=5,cex.lab=1.2, cex.axis=1.2, main="")




# ******
par(mfrow = c(2,2))
hist(nhanesv11fem$height,freq=FALSE, breaks=4, 
     main="Density histogram with 4 intervals", 
     xlab="Height (cm)", xaxp=c(120,200,16), col="coral")
hist(nhanesv11fem$height,freq=FALSE, breaks=6, 
     main="Density histogram with 6 intervals", 
     xlab="Height (cm)", xaxp=c(120,200,16), col="coral")
hist(nhanesv11fem$height,freq=FALSE, breaks=12, 
     main="Density histogram with 12 intervals", 
     xlab="Height (cm)", xaxp=c(120,200,16), col="coral")
hist(nhanesv11fem$height,freq=FALSE, breaks=28, 
     main="Density histogram with 28 intervals", 
     xlab="Height (cm)", xaxp=c(120,200,16), col="coral")
lines(density(nhanesv11fem$height, na.rm=TRUE, bw="SJ", adjust=2), lwd=2)

# *****************************************

nhanesva<-read.table("dataproject_datasets/nhanesdataset_a.tsv",header=T,sep="\t")
dim(nhanesva)
nhanesva$sex <- as.numeric(nhanesva$male)
nhanesvafem <- nhanesva[nhanesva$sex==0,]
dim(nhanesvafem)

hist(nhanesvafem$height,freq=FALSE, breaks=50, 
     main="Histogram: \n Height of Women (NHANES Study)", 
     xlab="height (cm)", xaxp=c(120,200,16))
plot(ecdf(nhanesvafem$height), verticals = TRUE, do.points = FALSE, 
     main = "Cum. Distribution: \n Height of Women (NHANES Study)", 
     xlab = "height (cm)",  yaxp=c(0,1,20), xaxp=c(120,200,16))
abline(h=seq(0,1,by=0.05))
boxplot(nhanesv11fem$height, main="Boxplot: \n Height of Women (NHANES Study)",
        ylab="height (cm)")
abline(h=seq(120,200,by=10))

mean(nhanesv11fem$height,na.rm=TRUE)
sd(nhanesv11fem$height,na.rm=TRUE)
quantile(nhanesv11fem$height,na.rm=TRUE, probs=c(0,0.25, 0.5, 0.75, 0.95, 1))

plot.stepfun(ecdf(nhanesv11fem$height), verticals = TRUE, do.points = FALSE, 
             main = "Cum. Distribution: \n Height of Women (NHANES Study)", 
             xlab = "height (cm)",  yaxp=c(0,1,20), xaxp=c(120,200,16))
par(mfrow=c(1,1))
#********************************************
#Lecture 5: Testing

# Loading ggplot2
install.packages("ggplot2")
library(ggplot2)

# Drawing a Normal Density Funktion
set.seed(1)
draws <- rnorm(10000)
dens <- density(draws, bw="SJ", adjust=2)
plot(dens, xlim= c(-5, 5), main="", xlab="", lwd=2, cex=2)



#Shading two areas in a density plot: 
q2     <- 2
q65    <- 6.5
qn08   <- -0.8
qn02   <- -0.2
x1 <- min(which(dens$x >= q2))  
x2 <- max(which(dens$x <  q65))
x3 <- min(which(dens$x >= qn08))  
x4 <- max(which(dens$x <  qn02))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray"))
with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="gray"))

# Shading of tails
q2     <- qnorm(0.975)
q65    <- max(dens$x)
qn08   <- min(dens$x)
qn02   <- qnorm(0.025)
x1 <- min(which(dens$x >= q2))  
x2 <- max(which(dens$x <  q65))
x3 <- min(which(dens$x >= qn08))  
x4 <- max(which(dens$x <  qn02))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray"))
with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="gray"))

q2     <- 2.3
q65    <- max(dens$x)
qn08   <- min(dens$x)
qn02   <- -2.3
x1 <- min(which(dens$x >= q2))  
x2 <- max(which(dens$x <  q65))
x3 <- min(which(dens$x >= qn08))  
x4 <- max(which(dens$x <  qn02))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="coral"))
with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="coral"))

#########################
# Type 1 and Type 2 errors and Power graph
draws <- rnorm(10000, mean=0, sd=1)
draws.right <- rnorm(10000, mean=4.5, sd=1)
densH0 <- density(draws, bw="SJ", adjust=2)
densH1 <- density(draws.right, bw="SJ", adjust=2)
plot(densH0, xlim= c(-5, 9), main="", xlab="", lwd=2, cex=2)
lines(densH1, xlim= c(-5, 5), main="", xlab="", lwd=2, cex=2)

# Left density curve
q2     <- qnorm(0.975)
q65    <- max(dens$x)
qn08   <- min(dens$x)
qn02   <- qnorm(0.025)
x1 <- min(which(dens$x >= q2))  
x2 <- max(which(dens$x <  q65))
x3 <- min(which(dens$x >= qn08))  
x4 <- max(which(dens$x <  qn02))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0,0,1,1/4)))
with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col=rgb(0,0,1,1/4)))

# Right density curve
dens.all = dnorm(x.grid,mean=mu, sd = sig)



with(dens, polygon(x=c(x[c(x5,x5:x6,x6)]), y= c(0, y[x5:x6], 0), col=rgb(1,0,0,1/4)))
with(dens, polygon(x=c(x[c(x7,x7:x8,x8)]), y= c(0, y[x7:x8], 0), col=rgb(1,0,0,1/4)))







