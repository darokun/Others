# Display the Student's t distribution and compare to the normal distribution

par(mfrow=c(1,1))
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 30)
colors <- c("red","black")
labels <- c("t (df=1)", "normal")

plot(x, hx, type="l", lty=1, xlab="x value", lwd=2,
     ylab="Density", main="Comparison of t distribution
against normal distribution")

for (i in 1:1){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", title="Distributions",
       labels, lwd=2, lty=c(1, 1), col=colors)

# End of Script