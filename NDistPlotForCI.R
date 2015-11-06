### normal distribution plots for confidence intervals (fig1)
par(mfrow=c(1,2), xpd=TRUE)

# first plot
cord.xl <- c(-3)
cord.yl <- c(0)
cord.xl <- c(cord.xl,-3) 
cord.yl <- c(cord.yl,dnorm(-3))
cord.xl <- c(cord.xl,-1.96,-1.96)
cord.yl <- c(cord.yl,dnorm(-1.96),0)
cord.xl <- c(-3,seq(-3,-1.96,0.01),-1.96) 
cord.yl <- c(0,dnorm(seq(-3,-1.96,0.01)),0) 


cord.xr <- c(3)
cord.yr <- c(0)
cord.xr <- c(cord.xr,3) 
cord.yr <- c(cord.yr,dnorm(3))
cord.xr <- c(cord.xr,1.96,1.96)
cord.yr <- c(cord.yr,dnorm(1.96),0)
cord.xr <- c(3,seq(3,1.96,-0.01),1.96) 
cord.yr <- c(0,dnorm(seq(3,1.96,-0.01)),0) 

cord.xc <- c(-1.96)
cord.yc <- c(0)
cord.xc <- c(cord.xc,-1.96) 
cord.yc <- c(cord.yc,dnorm(-1.96))
cord.xc <- c(cord.xc,1.96,1.96)
cord.yc <- c(cord.yc,dnorm(1.96),0)
cord.xc <- c(-1.96,seq(-1.96,1.96,0.01),1.96) 
cord.yc <- c(0,dnorm(seq(-1.96,1.96,0.01)),0) 

curve(dnorm(x,0,1),xlim=c(-3,3),main='Normal Distribution 
      for 95% CI', axes=FALSE, frame.plot=TRUE, xlab="", ylab="")
axis(side=1, labels=FALSE)

polygon(cord.xl,cord.yl,col='coral')
polygon(cord.xr,cord.yr,col='coral')
polygon(cord.xc,cord.yc,col=rgb(0,206,209,100,max = 255)) # col='dark turquoise' in rgb with alpha=100

text(0,0.2,"95%")
text(-2.5,0.1,"2.5%")
text(2.5,0.1, "2.5%")
lines(c(-2.25,-2.5), c(0.02,0.08))
lines(c(2.25,2.5), c(0.02,0.08))

text(-2, -0.05, "-1.96")
text(2, -0.05, "+1.96")
text(0, -0.05, "0")
text(-2, -0.1, "qnorm(0.025)")
text(2, -0.1, "qnorm(0.975)")

# second plot

curve(dnorm(x,0,1),xlim=c(-3,3),main='Normal Distribution
      for any CI', axes=FALSE, frame.plot=TRUE, xlab="", ylab="")
axis(side=1, labels=FALSE)

polygon(cord.xl,cord.yl,col='coral')
polygon(cord.xr,cord.yr,col='coral')
polygon(cord.xc,cord.yc,col=rgb(0,206,209,100, max = 255)) # col='dark turquoise' in rgb with alpha=100


# 1- alpha
text(-0.25,0.2,"1-")
text(0.25,0.197,expression(alpha))

# alpha/2 left
text(-2.95,0.1,expression(alpha))
text(-2.5,0.1,"/2")

# alpha/2 right
text(2.25,0.1,expression(alpha))
text(2.65,0.1, "/2")

lines(c(-2.25,-2.5), c(0.02,0.08))
lines(c(2.25,2.5), c(0.02,0.08))
lines(c(-2,-2),c(-0.02,-0.07))
lines(c(2,2),c(-0.02,-0.07))

text(0, -0.05, "??")

# qnorm(alpha/2) left
text(-3, -0.1, "qnorm(")
text(-2, -0.1, expression(alpha))
text(-1.5, -0.1, "/2)")

# qnorm(alpha/2) right
text(1, -0.1, "qnorm(1-")
text(2.35, -0.1, expression(alpha))
text(2.85, -0.1, "/2)")

# End of Script



