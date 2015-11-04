################################
# F-tests and the Chow test
#     
# uses IncomeByGroup.csv
# "year"  "Low20" "Mid60" "Top20"
# The World Top Incomes Database#   http://topincomes.parisschoolofeconomics.eu/
# Database:
# March  2015
################################

#Get the data;  ( you can use this to get any csv data)
d<-file.choose();
D<-read.csv(d);
names(D);
colnames(D);
colnames(D)<-c("Year","Low20","Mid60","Top20");#capitalizing the name of Year
attach(D);

par(mfrow=c(3,1))
plot(Low20, Year, col="dark green", pch=20)
plot(Mid60,Year,col="blue", pch=20)
plot(Top20, Year, col="red", pch=20)

#  Information needed 
n<- length(Year);
k <- 2  
Break<-1997;


#  Choose one of the following variables
Variable<-Top20; 
group<-"Top 20% of distribution";
Variable<-Mid60; 
group<-"Middle 60% of distribution";
Variable<-Low20;
group<-"Bottom 20% of distribution";

## Run the rest of of the program
# Parameters are restriced to being the same
R<-Variable~Year; 

# Unrestricted- two separate lines estimated
B<-Variable[Year<Break]~Year[Year<Break];
C<-Variable[Year>(Break-1)]~Year[Year>(Break-1)];

par(mfrow=c(1,1))
# plot the dataset and the 
plot(R);
abline(lm(R), lwd=4);
abline(lm(B), col="red");
abline(lm(C), col="blue");
title(main="Changes in Mean After-Tax Incomes", sub=group);

# Calculate the residuals
# Restricted sum of squared residuals
ssrR <- sum((resid(lm(R))^2));

# Unrestricted sum of squared residuals
ssr1<- sum((resid(lm(B))^2));
ssr2<- sum((resid(lm(C))^2));
ssrU<- ssr1+ssr2;

# Calculate the F statistic
Fstat<-((ssrR-ssrU)/k)/(ssrU/(n-2*k));

Fstat;    # Show the F statistic calculated
qf(.95,k,(n-2*k));  # Find the 95% confidence limit
1-pf(Fstat,k,(n-2*k));# Find the p-value
