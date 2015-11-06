# Confidence interval estimation

# Known variance
set.seed(42)                        # set the seed to always generate the same 'random' sequence
x <- rnorm(100,mean=1,sd=2)                 # generate sequence and store it in x
mean.x <- mean(x)                           # calculate mean of x and store in mean.x
n.x <- 100                                    # sample size is 100
sd.x <- 2                                     # standard deviation is known, and it is 2
low <- mean.x - qnorm(0.975)*sd.x/sqrt(n.x)     # low bound of the CI
up <- mean.x + qnorm(0.975)*sd.x/sqrt(n.x)      # high bound of the CI
c(low,up)                                   # look at both lower and upper bound of the confidence interval
mean.x

# Unknown variance
set.seed(42)                          # set the seed to always generate the same 'random' sequence
x <- rnorm(100,mean=1,sd=2)                   # generate sequence and store it in x
mean.x <- mean(x)                             # calculate mean of x and store in mean.x
n.x <- 100                                    # sample size is 100
sd.x <- sd(x)                                 # standard deviation is not known, so we estimate it from x
low <- mean.x - qnorm(0.975)*sd.x/sqrt(n.x)   # low bound of the CI
up <- mean.x + qnorm(0.975)*sd.x/sqrt(n.x)    # high bound of the CI
c(low,up)                                     # look at both lower and upper bound of the confidence interval
mean.x

# End of Script