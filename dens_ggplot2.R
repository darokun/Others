library(ggplot2)
x  = seq(-7, 10, length = 200)
y1 = dnorm(x, mean = 0,sd = 1)
y2 = dnorm(x, mean = 3,sd = 2)

mydf = data.frame(x, y1, y2)

head(mydf)

p0 = ggplot(mydf, aes(x = x)) +                         
  geom_line(aes(y = y1), colour = 'blue') +
  geom_line(aes(y = y2), colour = 'red') +
  geom_area(aes(y = pmin(y1, y2)), fill = 'gray60')
p0

##################
carrots <- data.frame(length = rnorm(100000, 6, 2))
cukes <- data.frame(length = rnorm(50000, 7, 2.5))

head(carrots)
head(cukes)

#Now, combine your two dataframes into one.  First make a new column in each.
carrots$veg <- 'carrot'
cukes$veg <- 'cuke'

head(carrots)
head(cukes)

#and combine into your new data frame vegLengths
vegLengths <- rbind(carrots, cukes)

head(vegLengths)
tail(vegLengths)

#now make your lovely plot
ggplot(vegLengths, aes(length, fill = veg)) + geom_density(alpha = 0.2)
