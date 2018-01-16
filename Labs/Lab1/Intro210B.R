##########################################################################################################
#
#   Introduction to data analysis with RStudio (R)
#   
#   Last Modified: January 11, 2018 by K.Goulias
########################################################################################################
#
#  The following defines your working directory
#
setwd("~/Desktop/geog210b")
#
#  you can verify your working directory with getwd
#
getwd()
# install a read file library and import in R the data from a csv file
#
library(readr)
#
#  this file contaoins California resident household records from a big travel behavior survey 
#  it has the typical structure of a social sciences or market analysis survey
#
SmallHHfile <- read_csv("SmallHHfile.csv")
#
# inspect the data we imported
#
View(SmallHHfile)
#
# display the data.frame
str(SmallHHfile)
#
# Provide a summary of the descriptive statsitics of all the variables
summary(SmallHHfile)
#
#
#
###########################################################################################################
#
#    Descriptive statistics with psych package
#
##########################################################################################################
#
install.packages("psych")  #Use this to install it, do this only once
library(psych)
describe(SmallHHfile)
#
# package psych also computes descriptive statistics by groups of one or more variables
# not very pretty but does the job
#
describeBy(SmallHHfile$HTRIPS, SmallHHfile$DOW)
#
# analysis that outputs a matrix of results and stores them in Mat1
#
Mat1 <- describeBy(SmallHHfile$HTRIPS,list(SmallHHfile$DOW,SmallHHfile$HHVEH),
                   mat=TRUE,digits=2)  #matrix output
# display the matrix Mat1
Mat1
#
###########################################################################################################
#  The following are plotting and data manipulation libraries
############################################################################################################
install.packages("plyer'")
library(plyr)
install.packages("memisc")
library(memisc)
install.packages("stargazer")  
library(stargazer)
install.packages("tidyverse")
install.packages("ggplot2")
library ("ggplot2")
library("dplyr")

#  ggplot is cool!



ggplot(SmallHHfile, aes(HHSIZ, HTRIPS, colour = INCOM)) + 
  geom_point()


m<-ggplot(SmallHHfile, aes(HTRIPS))  
m + geom_histogram(binwidth = 1)
m + geom_histogram(aes(fill = ..count..), binwidth = 1) + scale_fill_gradient("Count", low = "green", high = "red")

###############################################################################################################
# Using facets can show underlying heterogeneity
#############################################################################################################
m <- m + geom_histogram(binwidth = 1)
m + facet_grid(. ~ center)


############## More examples not used in lectures
#m + facet_grid(center ~ HHVEH)
# colored categories with fill aesthetic
#k <- ggplot(SmallHHfile, aes(factor(HTRIPS), fill = factor(HHVEH)))
#k + geom_bar()
# when the variables are continous
# Fill aesthetic can also be used with a continuous variable
#l <- ggplot(SmallHHfile, aes(x= TrpPrs, y=MilesPr))
#l + geom_point(aes(color = HHVEH))
#  representing the realtionship with a line and different color but don't do this imputes trends that don't exist
#l+ geom_line(aes(colour = HHVEH)) + scale_colour_gradient(low="red")
#  representing the realtionship with a line and different color
#l+ geom_line(aes(colour = HHVEH)) + scale_colour_gradient(low="green", high="black")
#
#
# colored categories in bars
#
k <- ggplot(SmallHHfile, aes(factor(HTRIPS), fill = factor(HHVEH)))
k + geom_bar()
#
#
#
################################################################################################################
#
# Boxplots
#
###############################################################################################################
# if you want to see all available options for a function put a ? in front and run it
?boxplot
#
boxplot(SmallHHfile$HTRIPS)
#
#
#
# switching off the drawing of outliers
boxplot(SmallHHfile$HHSIZ, outline = FALSE)
# show boxplots of household size for each group of household by different number of cars
boxplot(SmallHHfile$HHSIZ ~ SmallHHfile$HHVEH, outline = TRUE)
# Labeling the categories of household cars and the overall graph of boxplots
# Note how names = c() is used to assign labels
boxplot(SmallHHfile$HHSIZ ~ SmallHHfile$HHVEH, outline = TRUE, main="Household Cars by Household Size", names = c("0car","1car","2car","3car","4car","5car","6car","7car","8car"))
#
#
#  The following is more comlicated because I want to display a histogram and a boxplot in the same image
#
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
par(mar=c(3.1, 3.1, 1.1, 2.1))
hist(SmallHHfile$TotDist, col=rgb(0,1,0,0.9),breaks=400, xlim=c(0,800), 
     ylim=c(0,16000), xlab="Number of Travel Miles", main=" Household Vehicle Miles of Daily Travel")
boxplot(SmallHHfile$TotDist, horizontal=TRUE,  outline=TRUE,  ylim=c(0,800), frame=F, col = "green1")
box()
#
# same as above but no outliers for TotDist
#
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
par(mar=c(3.1, 3.1, 1.1, 2.1))
hist(SmallHHfile$TotDist, col=rgb(0,1,0,0.9),breaks=400, xlim=c(0,800), 
     ylim=c(0,16000), xlab="Number of Travel Miles", main=" Household Vehicle Miles of Daily Travel")
boxplot(SmallHHfile$TotDist, horizontal=TRUE,  outline=FALSE,  ylim=c(0,800), frame=F, col = "green1")
box()

##############################################################################################################
#  Measures of association
# pairwise correlations among all variables round to 2 decimal places
#
################################################################################################################
# pairwise covariances among all variables 
COVMAT = cov(SmallHHfile)
round(COVMAT, 3)


library(dplyr)  # install the library to allow easy subsetting of variables
names(SmallHHfile) # check the names of the columns
NEWFRAME <-select(SmallHHfile, INCOM:HTRIPS) # select the columns from INCOM to HTRIPS
corstats<-cor(NEWFRAME) # compute correlations of these few variables
round(corstats, 2) # print correlation with 2 decimal places
# pairwise covariances among all variables 
COVMAT = cov(NEWFRAME)
round(COVMAT, 3)


MONDAY <-filter(SmallHHfile, Mon == 1)
NEWMON <-select(MONDAY, INCOM:HTRIPS)
corstats<-cor(NEWMON)
round(corstats, 2)
covstats<-cov(NEWMON)
round(covstats, 2)

