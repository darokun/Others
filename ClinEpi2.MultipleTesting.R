########################
### Multiple Testing ###
########################

#########################
### Preliminary stuff ###
#########################

# When working from home
#setwd("~/Dropbox/Public/R wd")

# When working at KR5
#setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")

#------------------------------------------------------------------------------------#

#################
### Execution ###
#################

##################
### Exercise 1 ###
##################

#1 Simulate data

# diseases
diabetes<- rbinom(10000,1,prob=0.1)
asthma<- rbinom(10000,1,prob=0.1)
cancer<- rbinom(10000,1,prob=0.1)
ms<- rbinom(10000,1,prob=0.1)
thyroid<- rbinom(10000,1,prob=0.1)
liver<- rbinom(10000,1,prob=0.1)
arthritis<- rbinom(10000,1,prob=0.1)

# candidate risk factors
pisces<- rbinom(10000,1,prob=0.05)
purple<- rbinom(10000,1,prob=0.05)
from0to1<- rbinom(10000,1,prob=0.05)
redhair<- rbinom(10000,1,prob=0.05)
firstnameC<- rbinom(10000,1,prob=0.05)
summer<- rbinom(10000,1,prob=0.05)

#2 Chi-square tests

# diabetes
chisq.test(diabetes,pisces)$p.value
chisq.test(diabetes,purple)$p.value
chisq.test(diabetes,from0to1)$p.value
chisq.test(diabetes,redhair)$p.value
chisq.test(diabetes,firstnameC)$p.value
chisq.test(diabetes,summer)$p.value

# diab + red hair

# asthma
chisq.test(asthma,pisces)$p.value
chisq.test(asthma,purple)$p.value
chisq.test(asthma,from0to1)$p.value
chisq.test(asthma,redhair)$p.value
chisq.test(asthma,firstnameC)$p.value
chisq.test(asthma,summer)$p.value
#none

# cancer
chisq.test(cancer,pisces)$p.value
chisq.test(cancer,purple)$p.value
chisq.test(cancer,from0to1)$p.value
chisq.test(cancer,redhair)$p.value
chisq.test(cancer,firstnameC)$p.value
chisq.test(cancer,summer)$p.value
#none

# MS
chisq.test(ms,pisces)$p.value
chisq.test(ms,purple)$p.value
chisq.test(ms,from0to1)$p.value
chisq.test(ms,redhair)$p.value
chisq.test(ms,firstnameC)$p.value
chisq.test(ms,summer)$p.value
#none

# thyroid D
chisq.test(thyroid,pisces)$p.value
chisq.test(thyroid,purple)$p.value
chisq.test(thyroid,from0to1)$p.value
chisq.test(thyroid,redhair)$p.value
chisq.test(thyroid,firstnameC)$p.value
chisq.test(thyroid,summer)$p.value
#none

# liver
chisq.test(liver,pisces)$p.value
chisq.test(liver,purple)$p.value
chisq.test(liver,from0to1)$p.value
chisq.test(liver,redhair)$p.value
chisq.test(liver,firstnameC)$p.value
chisq.test(liver,summer)$p.value
#liver and from0to1

# arthritis
chisq.test(arthritis,pisces)$p.value
chisq.test(arthritis,purple)$p.value
chisq.test(arthritis,from0to1)$p.value
chisq.test(arthritis,redhair)$p.value
chisq.test(arthritis,firstnameC)$p.value
chisq.test(arthritis,summer)$p.value
#none


##################
### Exercise 2 ###
##################

# 1 Bonferroni correction
options(scipen=999) # Deactivate scientific notation
# Create the numeric vector of p.values
p <- c(0.0140, 0.2960, 0.9530, 0.0031, 0.1050, 0.6410, 0.7810, 0.9010, 0.0053, 0.4500)
p.bonferroni <- p.adjust(p, method = "bonferroni", n = length(p))
p.bonferroni

#2 and 3 Holm correction
p.holm <- p.adjust(p, method = "holm", n = length(p)) # w/o sorting
p2 <- sort(p) #sort
p2
p.holm <- p.adjust(p2, method = "holm", n = length(p))
p.holm


##################
### Exercise 3 ###
##################

#1 Load the data from the webpage
loc.url <- "http://www.ibe.med.uni-muenchen.de/organisation/mitarbeiter/020_professuren/boulesteix/clinepi/cll.zip"
td <- tempdir() 
tf <- tempfile(tmpdir=td, fileext=".zip") 
download.file(loc.url, tf) 
fname <- unzip(tf, list=F) 
cll <- load(fname)


View(CLL)
CLL[1:5,1:6]

#2 testing whether the mean expression of the gene is 
# equal between stable and progressive patients
y<-CLL[,1] # subset only variable y
X<-CLL[,-1] #subset ALL variables except y
pvalues<-numeric(12625) #empty vector for pvalues

# for loop for computing 12625 tests and p-values
# better than using apply (for me)
for (i in 1:12625) 
{
  pvalues[i]<-t.test(X[y=="progres.",i],X[y=="stable",i])$p.value
}

# 3
my.pvalues_bonf<-pvalues*length(pvalues)
# Set the p-values larger than 1 to 1.
my.pvalues_bonf[my.pvalues_bonf>1]<-1

# 4
# Number of rejected null-hypotheses without adjustment
sum(pvalues<0.05)
# [1] 740
# Number of rejected null-hypotheses with Bonferroni adjustment
sum(my.pvalues_bonf<0.05)
# [1] 0

# 5
pvalues_bonf<-p.adjust(pvalues,method="bonferroni")
pvalues_holm<-p.adjust(pvalues,method="holm")
pvalues_bh<-p.adjust(pvalues,method="BH")

# Compare the 20 smallest p-values 
# with Bonferroni adjustment
sort(pvalues_bonf)[1:20]
# with Bonferroni adjustment "by hand"
sort(my.pvalues_bonf)[1:20]
# with Holm adjustment
sort(pvalues_holm)[1:20]
# with Benjamini-Hochberg
sort(pvalues_bh)[1:20]

#sort(pvalues_bonf)[1:20]
#[1] 0.2057566 0.5124417
#[3] 0.8793838 1.0000000
#[5] 1.0000000 1.0000000
#[7] 1.0000000 1.0000000
#[9] 1.0000000 1.0000000
#[11] 1.0000000 1.0000000
#[13] 1.0000000 1.0000000
#[15] 1.0000000 1.0000000
#[17] 1.0000000 1.0000000
#[19] 1.0000000 1.0000000

#sort(my.pvalues_bonf)[1:20]
#[1] 0.2057566 0.5124417
#[3] 0.8793838 1.0000000
#[5] 1.0000000 1.0000000
#[7] 1.0000000 1.0000000
#[9] 1.0000000 1.0000000
#[11] 1.0000000 1.0000000
#[13] 1.0000000 1.0000000
#[15] 1.0000000 1.0000000
#[17] 1.0000000 1.0000000
#[19] 1.0000000 1.0000000

#sort(pvalues_holm)[1:20]
#[1] 0.2057566 0.5124011
#[3] 0.8792445 1.0000000
#[5] 1.0000000 1.0000000
#[7] 1.0000000 1.0000000
#[9] 1.0000000 1.0000000
#[11] 1.0000000 1.0000000
#[13] 1.0000000 1.0000000
#[15] 1.0000000 1.0000000
#[17] 1.0000000 1.0000000
#[19] 1.0000000 1.0000000

#sort(pvalues_bh)[1:20]
#[1] 0.2057566 0.2436177
#[3] 0.2436177 0.2436177
#[5] 0.2436177 0.3516678
#[7] 0.3516678 0.3516678
#[9] 0.3516678 0.3516678
#[11] 0.3516678 0.3516678
#[13] 0.3516678 0.3516678
#[15] 0.3516678 0.3516678
#[17] 0.3516678 0.3516678
#[19] 0.3516678 0.3516678

# 6
sum(pvalues_bonf<0.05)
sum(my.pvalues_bonf<0.05)
sum(pvalues_holm<0.05)
sum(pvalues_bh<0.05)
# all say zero

#####################
### END OF SCRIPT ###
#####################