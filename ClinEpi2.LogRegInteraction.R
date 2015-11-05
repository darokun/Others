#########################
### Preliminary stuff ###
#########################

# When working from home
# setwd("~/Dropbox/Public/R wd")

# When working at KR5
# setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")

#------------------------------------------------------------------------------------#

#################
### Execution ###
#################

###############################
### Log reg and interaction ###
###############################

### Models to be tested: - Slide 5
### log(Odds) = 1 + alkohol + tabak (m1)
### log(Odds) = 1 + alkohol + tabak + alkohol * tabak
### log(Odds) = 1 + alkohol * tabak (m2)

### Dataset construction - Slide 6
F <- c(10,11,13,9,7,16,50,16,4,18,60,27,5,21,125,91)
K <- c(38,26,36,8,27,35,60,19,12,16,49,14,8,20,52,27)
alkohol <- c(0:3)%x%rep(1,4)
tabak <- rep(c(0:3),4)

#Factorial - Slide 6 (and 15)
alkoholf <- as.factor(alkohol)
tabakf <- as.factor(tabak)

# Models m1 and m2 - Slide 6
m1 <- glm(cbind(F,K) ~ alkoholf + tabakf, family = binomial)
summary(m1)

m2 <- glm(cbind(F,K) ~ alkoholf * tabakf, family = binomial)
summary(m2)

# Likelihood Ratio test: anova m1 against m2 - Slide 7
anova(m1,m2,test="Chisq")

# Dummy coding for factorial coding - Slide 9
contr.treatment(4) 
# Check the contr.treatment function with ?contr.treatment for more info

### Testing the main effects - Slide 11
# log(Odds) = 1 + alkohol (m3)
# log(Odss) = 1 + tabak (m4)
#--------- against --------#
# log(Odds) = 1 + alkohol + tabak (m1)

# Models m3 and m4 - Slide 12
m3 <- glm(cbind(F,K) ~ alkoholf, family = binomial)
summary(m3)

m4 <- glm(cbind(F,K) ~ tabakf, family = binomial)
summary(m4)

# Likelihood Ratio test: anova m1 against m3, and m1 against m4 - Slide 12
anova(m1,m4,test="Chisq")
anova(m1,m3,test="Chisq")

### Dose-response coding - Slide 15
# Alternatives:
# Factorial (previously done): alkoholf and tabakf
# Ordinal categories: alkoholo and tabako
# Indices of single categories (0,1,2,3)
# Mean exposure (0,10,30,50 cigarrettes per day)

#------------------------------------------------------------------------------------#

### Ordinal Coding - Slide 17
alkoholo <- ordered(alkohol)
tabako <- ordered(tabak)
m5 <- glm(cbind(F, K)~alkoholo+tabako, family = binomial)
summary(m5)

# Dummy coding for ordinal coding - Slide 18
dummy.ord.raw <- contr.poly(4) # not rounded
dummy.ord <- round(contr.poly(4),3) #rounded
# Check the contr.poly function with ?contr.poly for more info

# Diagram Plot - Slides 18 and 19
y <- round(contr.poly(4),3)
y
y1 <- as.matrix(y[,1])
y1
y2 <- as.matrix(y[,2])
y2
y3 <- as.matrix(y[,3])
y3

plot(0:3, y1, type = "l", lwd = 2, col = "black", ylab = "Polynom.Contrasts")
lines(0:3, y2, lwd = 2, col = "red")
lines(0:3, y3, lwd = 2, col = "green")

# Euclidean norm self-constructed function: - Slide 18
# Note: The Euclidean norm  is the squared root of the sum of squared values.
# I couldn't find any function to do this, so I decided to construct an euc_norm 
# function. The same results can be achieved when using sqrt(sum(x^2)) where x
# is the vector for which we want to calculate the Euc. norm.
euc_norm <- function(x) sqrt(sum(x^2)) # Building the function

euc_norm(y1) # Applying the function on y1, y2 and y3
euc_norm(y2)
euc_norm(y3)
# For more info: http://bit.ly/1Kxzfax

# Scalar product function: - Slide 18
# Same as with the euc_norm function. The scalar product is equivalent to sum(x*y)
sca_prod <- function(x,y) sum(x*y)

sca_prod(y1,y2)
sca_prod(y1,y3)
sca_prod(y2,y3)

#------------------------------------------------------------------------------------#
# Interpretation of this part:
# I am not entirely sure I understand what we did with the Euc. norm and the scalar 
# product. I did find this paragraph on the wikipedia page for Orthogonality:

# "In certain cases, the word normal is used to mean orthogonal, particularly 
# in the geometric sense as in the normal to a surface. For example, the y-axis is 
# normal to the curve y = x2 at the origin. However, normal may also refer to the 
# magnitude of a vector. In particular, a set is called orthonormal (orthogonal plus 
# normal) if it is an orthogonal set of unit vectors. As a result, use of the term 
# normal to mean "orthogonal" is often avoided. The word "normal" also has a different
# meaning in probability and statistics."

# Plus, that same Wikipedia page says:
# "Two vectors, x and y, in an inner product space, V, are orthogonal if their 
# inner product (x,y) is zero."
# And the sca_prod function returns a value of zero between every vector.
# Source: http://en.wikipedia.org/wiki/Orthogonality
#------------------------------------------------------------------------------------#

###
### Calculate odds from the model - Slide 20 (two, three)
###

# Remember the Orthogonal Polynomials Dummy Coding Matrix:
constant <- 0.5
category <- 0:3
cbind(category, constant, dummy.ord)
dummy.ord.raw

# How it is on the slides:
# Ln(OR(2,3)) = -0.22574??0.5 +                    # SEE NOTE2 BELOW!!!
#                1.04828??alkoholo.L(2) + 
#                0.12234??alkoholo.Q(2) -
#                0.06348??alkoholo.C(2) + 
#                1.04261??tabako.L(3) ???
#                0.10308??tabako.Q(3) + 
#                0.02167??tabako.C(3) 

# Which is the same as:
ln.odds.two.three = -0.22574*0.5 + 
  1.04828*0.2236068 + 
  0.12234*(-0.5) -
  0.06348*(0.6708204) + 
  1.04261*0.6708204 - 
  0.10308*0.5 + 
  0.02167*0.2236068 
# Note1: "two" and "three" correspond to categories two for alcohol (0.4 - 1.5 oz/day)
# and three for cigarrettes (40+ cigarrettes/day) as seen on the table in slide 3.

# Note2: I disagree with the nomenclature here. On the slides, it appears like OR
# (odds ratio), but we're actually calculating just the odds.
ln.odds.two.three
round(exp(ln.odds.two.three), 3) # Back-transformation to normal scale
# The odds on slide 3, for categories two (alcohol) and three (cigarrettes) is 1.93


###
### Calculate odds from the model - Slide 21 (zero, zero)
###

# How it is on the slides:
# Ln(OR(2,3)) = -0.22574??0.5 +                    # SEE NOTE2 ABOVE!!!
#                1.04828??alkoholo.L(0) + 
#                0.12234??alkoholo.Q(0) -
#                0.06348??alkoholo.C(0) + 
#                1.04261??tabako.L(0) ???
#                0.10308??tabako.Q(0) + 
#                0.02167??tabako.C(0) 

# Which is the same as:
ln.odds.zero.zero = -0.22574*0.5 - 
  1.04828*0.6708204 + 
  0.12234*0.5 +
  0.06348*(0.2236068) -  
  1.04261*0.6708204 - 
  0.10308*0.5 - 
  0.02167*0.2236068 
ln.odds.zero.zero
round(exp(ln.odds.zero.zero), 3) # Back-transformation to normal scale
# The odds on slide 3, for categories zero (alcohol) and zero (cigarrettes) is 0.26

###
### OR between two.three and zero.zero - Slide 21
###

OR.2.3.0.0 <- round(round(exp(ln.odds.two.three), 3) / 
                      round(exp(ln.odds.zero.zero), 3), 2)
OR.2.3.0.0
round(1.93 / 0.26, 3) # Observed OR (from table on slide 3)

#------------------------------------------------------------------------------------#

### O, 1, 2, 3 Coding - Slide 22
m6 <- glm(cbind(F, K)~alkohol+tabak, family = binomial)
summary(m6)
anova(m5,m6,test="Chisq")
round(exp(0.45172), 3)
round(exp(0.49009), 3)
# Interpretation:
# With alcohol consumption fixed, a transition to the next higher smoking category
# increases the risk by an OR of exp(0.45172) = 1.571.
# With tobacco consumption fixed, a transition to the next higher alcohol category
# increases the risk by an OR of exp(0.49009) = 1.632.

#------------------------------------------------------------------------------------#

###
### Mean Value Coding - Slide 24
###

alkoholm<-rep(c(0.0, 0.2, 1.0, 2.0),rep(4,4))
tabakm<-rep(c(0, 10, 30, 50),4)
m7 <- glm(cbind(F, K)~alkoholm+tabakm, family = binomial)
summary(m7)

# Interpretation: (Tobacco)
# Given a fixed alcohol consumption, the odds of developing cancer of the
# oral cavity are changed by the factor exp(0.0253) = 1.0256 per daily
# smoked cigarette. This is a change of exp(0.0253) ??? 1 = 1.0256 ??? 1 = 2.56%.

#------------------------------------------------------------------------------------#

###
### Testing the deviation from a linear trend - Slide 29
### (Linear vs. Quadratic)
###

m8 <- glm(formula = cbind(F, K) ~ alkohol + I(alkohol^2) +
            tabak + I(tabak^2), family = binomial)
anova(m6,m8,test="Chisq")
# "There is no evidence that the quadratic terms are necessary to explain the data."

#------------------------------------------------------------------------------------#

###
### The category "no exposure"   - Slide 30
###

no.smoker<-ifelse(tabak==0,1,0)
m9<-glm(cbind(F, K) ~ alkohol + no.smoker + tabak, family = binomial)
anova(m6,m9,test="Chisq")


#####################
### END OF SCRIPT ###
#####################