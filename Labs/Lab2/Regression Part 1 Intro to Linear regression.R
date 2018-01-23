##########################################################################################################
#
#   Source code for linear regression models lecture notes 
#   Last Update January 20 2017 by K Goulias
#   Part 1 First linear regression model 
#          Hypothesis testing and 
#          Basic diagnostics
#          Illustration of dummy variables
#
########################################################################################################
########################################################################################################
# Read in R a comma delimited file (csv)  for Geog210b
#
# Note how I specify the directory in which the file is stored
# The file contains 42431 rows (each row is a household) and 31 variables
# Each column is a variable
# Modify this to match your working directory
######################################################################################################

SmallHHfile <- read.csv("~/Desktop/geog210b/SmallHHfile.csv", header=TRUE)

# Look at the structure of the file

str(SmallHHfile)

#Loook at the contents of the file - this will show in the 

View(SmallHHfile)

# Provide a summary of the descriptive statistics of all the variables

summary(SmallHHfile)

###########################################################################################################
#
#    Descriptive statistics
#
##########################################################################################################

install.packages("stargazer")  
library(stargazer)
stargazer(SmallHHfile, type = "text", title="Descriptive statistics", median=TRUE, digits=2, out="table1.txt")
#
#  The model in slides 28 and 29 from lecture
HHPMT.lm = lm(TotDist ~ HHSIZ , data=SmallHHfile)
summary(HHPMT.lm)
anova(HHPMT.lm) # for slide 33 in lecture notes


################################################################################################################
#
#   Simple comparison of means and sample size example (about slide 70 of intro to regression)
#
#############################################################################################################
library(psych)
describe(SmallHHfile$MilesPr) # descriptive stats for variable MilesPr

# this shows the descriptives of variable MilesPr in the SmallHHfile are:
#   vars     n   mean    sd   median trimmed   mad min     max   range skew kurtosis   se
#  X1    1 42431 27.12 43.46   14.5    18.4 18.19   0 1167.65 1167.65 5.15    47.24 0.21
#  mu0 = 27.12 sigma=43.46

TEN <- SmallHHfile[sample(nrow(SmallHHfile), 10), ] # randomly draw a sample of ten rows from SmallHHfile
describe(TEN$MilesPr)

# this shows the descriptives of variable MilePr in the TEN sample are:
#vars    n  mean    sd median trimmed  mad min   max range skew kurtosis   se
#X1    1 10 18.05 21.87  12.75   12.95 8.97   0 76.88 76.88 1.83     2.33 6.92
# xbar10 = 18.05  n10=10

# we treat the SmallHHfile like if it is the population with mu0 = 27.12 sigma=43.46

# first the TEN sample

xbar10 = 18.05            # sample mean 
mu0 = 27.12             # hypothesized value 
sigma = 43.46            # population standard deviation 
n = 10                 # sample size 

z10 = (xbar10-mu0)/(sigma/sqrt(n)) 
z10                      # test statistic for TEN
# the above shows a z = -0.6599599

# compute critical values at 5% significance

alpha = .05 # set the confidence level we want
z.half.alpha = qnorm(1 - alpha/2) # area under the normal curve 
c( -z.half.alpha, z.half.alpha)  #critical values

# The above produced the values −1.9600  1.9600

# The test statistic -0.6599599 lies between the critical values -1.9600 and 1.9600. 
# Therefore, at .05 significance level, we do not reject the null hypothesis that the mean of the TEN sample of MilesPr 18.02
# is the same as the population mean of 27.12.

HUNDRED <- SmallHHfile[sample(nrow(SmallHHfile), 100), ]  # randomly draw a sample of 100 rows from SmallHHfile
describe(HUNDRED$MilesPr)


# this shows the descriptives of variable MilePr in the HUNDRED sample are:
#  vars   n  mean    sd median trimmed   mad min   max range skew kurtosis  se
#X1    1 100 26.66 44.04  15.54   18.21 20.07   0 362.8 362.8 4.83    31.87 4.4
# xbar100=26.66 n100=100


# Test the hypothesis that the average in the HUNDRED random draws is the same as the average in the entire SmallHHfile 

xbar100 = 26.66            # sample mean 
mu0 = 27.12             # hypothesized value 
sigma = 43.46            # population standard deviation 
n = 100                 # sample size 

z100 = (xbar100-mu0)/(sigma/sqrt(n)) 
z100                      # test statistic for HUNDRED
# the above shows a z = -0.1058445

# The test statistic -0.1058445 lies between the critical values -1.9600 and 1.9600. 
# Therefore, at .05 significance level, we do not reject the null hypothesis that the mean of the TEN sample of MilesPr 26.66
# is the same as the population mean of 27.12.




THOUSAND <- SmallHHfile[sample(nrow(SmallHHfile), 1000), ]  # randomly draw a sample of 1000 rows from SmallHHfile
describe(THOUSAND$MilesPr)

# Test the hypothesis that the average in the THOUSAND radom draws is the sample is the average in the entire SmallHHfile 
#vars     n   mean    sd median trimmed  mad min    max  range skew kurtosis   se
#X1    1 1000 24.76 39.43   13.4   16.88 16.5   0 488.07 488.07 5.05    40.67 1.25
# xbar1000=24.76 n1000=1000


# Test the hypothesis that the average in the THOUSAND random draws is the same as the average in the entire SmallHHfile 


xbar1000 = 24.76            # sample mean 
mu0 = 27.12             # hypothesized value 
sigma = 43.46            # population standard deviation 
n = 1000                 # sample size 

z1000 = (xbar1000-mu0)/(sigma/sqrt(n)) 
z1000                      # test statistic for TEN
# the above shows a z = -1.717206

# The test statistic -1.717206 lies between the critical values -1.9600 and 1.9600. 
# Therefore, at .05 significance level, we do not reject the null hypothesis that the mean of the TEN sample of MilesPr 24.76
# is the same as the population mean of 27.12.


########################################################################################################################################
#
# WHEN YOU DON'T KNOW THE SIGMA IN THE POPULATION
#
############################################################################################################################################
#  The ten sample descriptives
#vars    n  mean    sd median trimmed  mad min   max range skew kurtosis   se
#X1    1 10 18.05 21.87  12.75   12.95 8.97   0 76.88 76.88 1.83     2.33 6.92
# xbar10 = 18.05, s10=21.87  n10=10



# first the TEN sample

xbar10 = 18.05            # sample mean 
mu0 = 27.12             # hypothesized value 
s10 = 21.87            # sample standard deviation 
n = 10                 # sample size 

t10 = (xbar10-mu0)/(s10/sqrt(n)) 
t10                      # test statistic for TEN
# the above shows a z = -1.31147

alpha = .05 
t.half.alpha = qt(1 - alpha/2, df=n-1) 
c(- t.half.alpha, t.half.alpha) 

# this gives you the two critical values  -2.262157  2.262157 with n-1 = 9


# again we cannot reject the H0 that the hypothesized value of 27.12 is the same as the sample value of 18.05

#################################################################################################
#
#                             Linear regression models in R (one explanatory variable)
#            See also http://www.statmethods.net/stats/regression.html
##################################################################################################
# we run a regression model and store it in a vector slide 79 in lecture notes

HHPMT.lm = lm(TotDist ~ HHSIZ , data=SmallHHfile)
summary(HHPMT.lm)

output <- summary(HHPMT.lm) 		    #	most important stats
#SSR <- deviance(HHPMT.lm) 			    # sum of squared residuals
#LL <- logLik(HHPMT.lm) 			    	# log likelihood(later)
#DegreesOfFreedom <- HHPMT.lm$df    #	degrees of freedom
# Yhat <- HHPMT.lm$fitted.values    #	the dep. var. predictions
#Coef <- HHPMT.lm$coefficients 			  # coefficient estimates
# Resid <- HHPMT.lm$residuals 			# residuals
#s <- output$sigma 			#	estimate of sigma of error (positive squared root of mean squared error whoch is susm of squared residual divided by df)
#RSquared <- output$r.squared #			Goodness of Fit
#CovMatrix <- s^2*output$cov 	#		covariance of coef.est.
# aic <- AIC(HHPMT.lm) #			 	AIC = −2logL(p)+2p 
# sbc <- AIC(HHPMT.lm,k=log(NROW(SmallHHfile))) # 	SBC =−2logL(p)+plog(N)
#nanova(HHPMT.lm)  # verify the above using the anova function

HHPMT.fit = fitted(HHPMT.lm)
plot(SmallHHfile$HHSIZ, HHPMT.fit, type="b" )  # type b means points and lines
summary(HHPMT.fit)

dev.off()

C

?plot.lm

plot(TotDist ~ HHSIZ, data = SmallHHfile)
abline(HHPMT.lm)



# Better looking graphs for regression models

require(ggplot2)
cool1 <- ggplot(SmallHHfile, aes(x=HHSIZ, y=TotDist)) + geom_point(shape=1) + geom_smooth(method=lm)
cool1 <- cool1 + scale_x_continuous(name = "Household Size") +
  scale_y_continuous(name = "Miles Travelled in a Day by each household")
cool1

# add a title to the graph
cool1 <- cool1+ ggtitle("Miles travelled Regression Line in CHTS 43431 households")

# use annotate to add a box with the regression coefficients and R square
cool1<- cool1 + annotate("rect", xmin = 7, xmax = 8, ymin = 4000, ymax = 6000, fill="white", colour="red") +
  annotate("text", x=7.5, y=4500, label = "R^2 == 0.06", parse=T) + annotate("text", x=7.5, y=5500, label = "alpha == 12.2", parse=T) +
  annotate("text", x=7.5, y=5000, label = "beta == 21.7", parse=T)
cool1


# The following will create an object called equation fro the linear regression lm object and also extract the r-square

equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 3),
                  b = round(coef(x)[2], digits = 3),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

# then we insert in the cool graph the equation from the object lm

cool1 <- cool1 + annotate("text", x = 2, y = 5000, label = equation(HHPMT.lm), parse = TRUE)
cool1


# ther is also another library that uses ggplot and extracts default graphs from an lm object

install.packages("ggfortify")
library(ggfortify)
autoplot(HHPMT.lm, label.size = 3)




###########################################################################################################
#
#  Illustration of dummy variables
#
###########################################################################################################

library(ggplot2)
library(dplyr)


# work with a data fram called id

id <- SmallHHfile %>% tibble::rownames_to_column() %>% as_data_frame()

# create a new variable FDOW that is a factor and has labelsfr days of the week- we will use this to group means of TotDist with proper labels

id <- id %>% mutate(FDOW = factor(DOW, levels = c(1,2,3,4,5,6,7),labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) 

# get the average values of TotDist by each daya of the wekk using the newly defined factor FDOW

id

gd <- id %>% 
  group_by(FDOW) %>% 
  summarise(TotDist = mean(TotDist))

# this will show that gd is a simple object of summaries
gd

# this is only the gd object stat="identity" is used to plot the value of the variable TotDist which is the average

ggplot(gd, aes(x = FDOW, y = TotDist)) +
  geom_bar(stat = "identity")

#  the following is the ggplot of the observed data and their averages in the gd object

ggplot(id, aes(x = FDOW, y = TotDist)) +
  geom_point() +
  geom_bar(data = gd, stat = "identity")

########################################################################################################
#  The regression model with dummy variables
##############################################################################################################

HHPMT2.lm = lm(TotDist ~ Mon + Tue + Wed + Thu + Fri+ Sat  , data=SmallHHfile)
summary(HHPMT2.lm)

HHPMT2.fit = fitted(HHPMT2.lm)
summary(HHPMT2.fit)
DOW

plot(HHPMT2.fit ~ DOW, data = SmallHHfile, xlim = c(1, 7), ylim = c(40, 100), panel.first = grid())

plot(HHPMT2.fit ~ FDOW, data = SmallHHfile, xlim = c(1, 7), ylim = c(40, 100), panel.first = grid())


# we can get te same diagnostics as any other model

library(ggfortify)
autoplot(HHPMT2.lm, label.size = 3)





