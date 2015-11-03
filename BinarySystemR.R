### Understanding the binary system using R

# Step 1:
len <- 8                          # Set length of vector to 8
x <- numeric(len)                 # generate an empty numeric vector of length 8 that's called x
x                                 # x is made up of 8 positions, temporarily occupied by 0's:
x[1] <- 1                         # manually set first position to be 1
x[2] <- 2                         # manually set second position to be 2

for (i in 3:len) {                # for loop to construct the rest of the sequence
  x[i] <- x[i-1]*2
} 

x                                 # the sequence follows the binary positional notation, but we want it backwards
x <- sort(x, decreasing=TRUE)     # sort the sequence in decreasing order
x                                 # now we get what we wanted!

# Step 2:
y <- c(1,1,0,0,1,0,0,0)
y

# Step 3:
rbind(x,y)

# Add everything:
128+64+8