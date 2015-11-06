# 1. Set wd (if needed)
# 2. Read in the data:
tab <- read.table("~/Dropbox/Public/R wd/dataproject_datasets/nhanesdataset_a.tsv", header=TRUE, sep="\t")
# 3. attach data:
attach(tab)

# Four different ways:
# 1: using logical operators (the one Riccardo and Vindi recommend)

age.cat1 <- NULL                          # I create a new empty vector called age.cat1
age.cat1[age >= 20 & age <= 34] <- 1      # I set the first age level to be between 20 and 34
age.cat1[age >= 35 & age <= 49] <- 2      # Second level: 35-49
age.cat1[age >= 50 & age <= 64] <- 3      # Third level: 50-64
age.cat1[age >= 65 & age <= 79] <- 4      # Fourth level: 65-79
age.cat1[age >= 80] <-5                   # Fifth level: more than 80
age.cat1 <- as.factor(age.cat1)           # I convert age.cat1 into a factor
tab$age.cat1 <- age.cat1                  # I add age.cat1 to my dataset as tab$age.cat1
summary(age.cat1)                         # check variable

#----

# 2: using the function cut()
# I create labels for each age group
age.labels <- c("20-34", "35-49", "50-64", "65-79", ">80")      
# I cut the variable age according to my desired points and label each resulting group with age.labels. Plus, I put everything directly into a variable called age.cat2, and add it to my dataset as tab$age.cat2
tab$age.cat2 <- cut(age, c(19, 34, 49, 64, 79, Inf ),
                    labels = age.labels)
# check variable:
summary(tab$age.cat2)

#----

# 3: using the recode() function, available from the car package.
install.packages("car")
library(car) #Load car package for using function recode

# use recode() to recode the age variable into 5 categories with desired cut-off points, make it a factor, and add it to my data set as tab$age.cat3:
tab$age.cat3 <- recode(age, "20:34='1'; 35:49='2'; 50:64='3'; 65:79='4'; 80:Inf='5' ", as.factor=T)

# check variable:
summary(tab$age.cat3)

#----
# 4 Using for loops and if statements:
age.cat4 <- NULL # create variable

for(i in 1:nrow(tab)) {                               # initiate for loop
  if(age[i] >= 20 & age[i] <= 34) age.cat4[i] <- 1    # first level
  if(age[i] >= 35 & age[i] <= 49) age.cat4[i] <- 2    # second level
  if(age[i] >= 50 & age[i] <= 64) age.cat4[i] <- 3    # third level
  if(age[i] >= 65 & age[i] <= 79) age.cat4[i] <- 4    # fourth level
  if(age[i] >= 80) age.cat4[i] <- 5                   # fifth level
}
age.cat4 <- as.factor(age.cat4)                       # convert to factor 
tab$age.cat4 <- age.cat4                              # add to data set as tab$age.cat4
summary(age.cat4)                                     # check variable

#----

# Check that all four new variables are equivalent:
rbind(table(age.cat1), table(age.cat2), table(age.cat3), table(age.cat4))

# End of Script
