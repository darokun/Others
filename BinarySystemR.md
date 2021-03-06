# Understanding the binary system using R
Daloha Rodr&iacute;guez-Molina  
November 3, 2015  

## Binary System (or base-2)
  The binary system is mostly used by computers, and could take one of two possible values: either `0` or `1`. To represent a quantity like `200`, the binary system uses [positional notation](https://en.wikipedia.org/wiki/Positional_notation), which means that it uses "the same symbol for the different orders of magnitude (for example, the "ones place", "tens place", "hundreds place")" (See [Wikipedia](https://en.wikipedia.org/wiki/Positional_notation)). 

  Unlike the [decimal base system](https://en.wikipedia.org/wiki/Decimal) (a.k.a. base-10, the one that humans use to count, and that goes from `0` to `9`), the "places" do not increase by times ten, but by times two. It means that the positions are "ones place", "twos place", "fours place", etc. Afterwards, the positon values are added together in order to get the number we want in base-10 (although computers understand only the zeros and ones). Plus, binary code is read from right to left.

  To try to see this more clearly, I will construct a table of position values for the binary system (1,2,4,8...), and assign either `0` or `1` to the positions until I get the number `200` in binary system:

* 1. Generate a numeric vector called `x`, with `length = 8` that increases by a factor of two, and that increases its value from right to left. The length should be eight in this case because the binary number I want to recreate has eight positions in binary:

```r
len <- 8                          # Set length of vector to 8
x <- numeric(len)                 # generate an empty numeric vector of length 8 that's called x
x                                 # x is made up of 8 positions, temporarily occupied by 0's:
```

```
## [1] 0 0 0 0 0 0 0 0
```

```r
x[1] <- 1                         # manually set first position to be 1
x[2] <- 2                         # manually set second position to be 2

for (i in 3:len) {                # for loop to construct the rest of the sequence
  x[i] <- x[i-1]*2
} 

x                                 # the sequence follows the binary positional notation, but we want it backwards
```

```
## [1]   1   2   4   8  16  32  64 128
```

```r
x <- sort(x, decreasing=TRUE)     # sort the sequence in decreasing order
x                                 # now we get what we wanted!
```

```
## [1] 128  64  32  16   8   4   2   1
```

* 2. We manually generate a `y` vector which contains the number `200` in binary (`11001000`):

```r
y <- c(1,1,0,0,1,0,0,0)
y
```

```
## [1] 1 1 0 0 1 0 0 0
```

* 3. Finally, we construct a table, where values of `y` correspond to their positions in `x`:

```r
rbind(x,y)
```

```
##   [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## x  128   64   32   16    8    4    2    1
## y    1    1    0    0    1    0    0    0
```

As seen, the positions where the ones are in the binary system correspond to values `128`, `64` and `8`. If we sum up these values we obtain:

```r
128+64+8
```

```
## [1] 200
```

And that's why 11001000 = 200 &#9786;

* If you want to recreate this in R, check the code at [this link](https://github.com/darokun/Others/blob/master/BinarySystemR.R).

#### Bonus:
* We can count from zero to five in base-10 with the fingers of one hand, but in binary we can count up to 31 using the same five fingers!
* For more info, and further explanations, check out the first 2 minutes of this cool YouTube video: [Binary Numbers and Base Systems as Fast as Possible](https://www.youtube.com/watch?v=LpuPe81bc2w)

###### End of Script


