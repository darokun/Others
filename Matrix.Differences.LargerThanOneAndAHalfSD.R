### Script for Miryam


# you may skip these two steps
# generating 102 (6*17) random values of mean 5 and sd 1.5
set.seed(1234)
x <- rnorm(102, 5, 1.5)

# building a matrix from x, with 6 rows and 17 columns
data <- matrix(x,nrow=6,ncol=17)

# you may start here
# sorting out the first column
sort(data[,1])

# create a vector to store differences
difference <- NULL

# calculating the differences: abs(x1-x2), and so on, until abs(x5-x6)
for(i in 1:(length(data[,1])-1)) {
  difference[i] <- abs(data[,1][i] - data[,1][i+1])
}

# create a boolean vector to store whether or not the differences are >=1.5 sd of this first column or not
sd_1.5 <- NULL

for(i in 1:length(difference)) {
  if(difference[i] >= sd(data[,1])*1.5) {
    sd_1.5[i] <- TRUE
  } else {
    sd_1.5[i] <- FALSE
  }
}




