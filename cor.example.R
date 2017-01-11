#-----
# CORRELATION EXAMPLE

# generate random data
set.seed(350)
n.posts <- abs(round(rnorm(n = 30, mean = 350, sd = 350), 0))
set.seed(700)
n.followers <- abs(round(rnorm(n = 30, mean = 350, sd = 350), 0))

# show data
cbind(n.posts, n.followers)

# scatterplots
par(mfrow = c(2,1), pch = 20)
plot(n.posts)
plot(n.followers)

# perform correlation (no correlation, not significant)
# cor(n.posts, n.followers)
cor.test(n.posts, n.followers)
x <- cor.test(n.posts, n.followers)
x
str(x)
round(x$estimate,2)*100
round(x$p.value,2)
round(x$conf.int,2)[1]
round(x$conf.int,2)[2]

# sort in ascending order (positive correlation)
n.posts <- sort(n.posts)
n.followers <- sort(n.followers)
cbind(n.posts, n.followers) # show data
plot(n.posts) # scatterplots
plot(n.followers)
# cor(n.posts, n.followers) # perform correlation (very high positive correlation)
cor.test(n.posts, n.followers) # (highly significant)

# sort n.posts in descending order (negative correlation)
n.posts <- sort(n.posts, decreasing = TRUE)
n.followers <- sort(n.followers)
cbind(n.posts, n.followers) # show data
plot(n.posts) # scatterplots
plot(n.followers)
# cor(n.posts, n.followers) # perform correlation (very high negative correlation)
cor.test(n.posts, n.followers) # (highly significant)

# LINEAR REGRESSION
lm.ig <- lm(n.followers ~ n.posts)
summary(lm.ig)

devtools::install_github("rstudio/rmarkdown")
library(rmarkdown)
