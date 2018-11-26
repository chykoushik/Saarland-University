### Stats with R Exercise sheet 5

##########################
#Week6: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 25. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Koushik Chowdhury
## Matriculation number: 2572865


##Group Members: 
## Name: Ashima Jindal
## Matriculation No: 2573144
## Name: Rayhanul Islam Rumel
## Matriculation No:2576541  


## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################

###############
### Cleaning Data
###############

library(lsr)
library(tidyr)
library(effsize)
library(lattice)
library(plyr)
library(Rmisc)
library(doBy)


# set your wd and load the data frame digsym_clean.csv
setwd("C:/Users/Koushik/Desktop")
getwd()
dat <- read.csv("digsym_clean.csv")
dat
# get rid of the column "X"

dat <- dat[, -2]
dat <- as.data.frame(dat)
dat

# Say you're interested in whether people respond with different accuracy to 
# right vs wrong picture-symbol combinations.
# In other words, you want to compare the average accuracy for the digsym-right 
# and digsym-wrong condition.
# Like the conscientious researcher you are, you want to take a look at the data 
# before you get into the stats.
# Therefore, you will need to create a barplot of the mean accuracy data 
# (split out by condition) using ggplot and the summarySE function (given below).
# Let's do it step by step.

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  # data: an input dataframe
  # measurevar: a column name of <data> (as string), on which we would like to calculate standard 
  #             deviation (SD), standard error (SE) and confidence interval (CI).
  # groupvars: categorical columns of <data> (as vector of strings ) which we would like to use
  #            to make all possible combinations for which we calculate SD, SE, CI based 
  #            on <measurevar>.
  # na.rm: should we remove NA
  # conf.interval: confidence interval
  library(doBy)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# apply the function summarySE on the accuracy data grouping by right/wrong condition
# (use the provided documentation inside the function for the arguments description)

dat1 <- summarySE(data = dat, measurevar = "accuracy", groupvar = "condition",  na.rm = FALSE, conf.interval = 0.95)
dat1
# Create the barplot (use ggplot2 for this and all tasks below) with error bars 
# (which the function summarySE readily provided).
# Gauging from the plot, does it look like there's a huge difference in accuracy 
# for responses to the right and wrong condition?
library(ggplot2)

ggplot(dat1, aes(condition, accuracy)) +
  geom_bar(stat = "identity") +  geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se), width=.2,position=position_dodge(.9))


#Comment: Here we are plotiing the error bars as se. By looking at the graph, we may conlude by saying that
# right condition has more acccuracy than left condition but it will be confirmed by doing statistical tests
# which are available.


# Let's go back to our data frame "data", which is still loaded in your console
# Now that you've taken a look at the data, you want to get into the stats.
# You want to compute a t-test for the average accuracy data in the right and 
# wrong condition.
# Why can't you compute a t-test on the data as they are now? 
# Hint: which assumption is violated?
head(dat)
#Comment:After looking at the data  we can say that accuracy  per subject is related to right and wrong condition.
## So we will not be able to compute t test because t test assumption  of independent data is getting violated.

# we need to reshape( - cast) the data to only one observation (average accuracy)
# per subject and right/wrong condition 
# Collapse the data, using 
# cast(data, var1 + var2 + var3 ... ~, function, value = var4, na.rm = T)
library(reshape2)
datcast<-dcast(dat, Subject+ condition ~., mean, value.var = "accuracy", na.rm = T)
datcast
# Create a histogram of the accuracy data depending on the right and wrong 
# condition and display them side by side
hist(datcast$.)

# Display the same data in a density plot 

d<-density(datcast$.)
plot(d)
# Based on the histograms and the density plots - are these data normally 
# distibuted?

## Looking at the histogram and density plot, it is being cleared that these data are
## negatively skewed and not normally distributed.

# Create a boxplot of the accuracy data
help(boxplot)
boxplot(datcast$.~datcast$condition,datcast)

# Compute the t-test to compare the mean accuracy between wrong and right picture
# combinations.
# Do you need a paired t-test or independent sample t-test? why?
help("t.test")
## we need a paired t test because the both mean accuracy and condition values are dependent on each other.
## here we will perform the  paired t test
help("t.test")
res1<-t.test(datcast$.~ datcast$condition, paired = TRUE)
res1
# What does the output tell you? What conclusions do you draw?
## The p value of the test is coming out to be 0.000588 which is less than significance level alpha=0.05.
##We can then reject the null hypothesis and conclude that average accuracy for right condition is significantly differed from Wrong condition


# Compute the effect size using CohensD 
??cohensD
library(lsr)
cohensD(datcast$.~datcast$condition,method = "paired")

# How big it is? How do you interpret this result?
# The value of d is  0.6196291 coming from cohensD formula. We can say that this value lies between medium and large effect size 
# having values 0.5 and 0.8 respectively. So for this effect size we will be able to see the difference by our naked eye.


# In addition to the long-format data we've just been working on, you may also 
# encounter data sets in a wide format 
# (this is the format we have been using in class examples.)
# Let's do a transformation of our data set to see how it would like in a wide 
# format.
# Use "spread" in tidyr.
data_wide <- spread(datcast, condition, .)
data_wide
# Compute the t test again on the wide format data - note that for wide-format 
# data you need to use a different annotation for the t-test.

res2<- t.test(data_wide$right, data_wide$wrong, paired=TRUE)
res2
# Compare the t-test results from the wide-format and the long-format data.
res2 #wide format
res1 # long format
# both  t test results in long and wide format are coming same.
# Compute CohensD on the wide format data.

cohensD(data_wide$right, data_wide$wrong, method = "paired")

# Let's try the t-test again, but for a different question:
# Suppose you are interested in whether reaction times in the digit symbol 
# task differ depending on gender.
# In other words, you want to test whether or not men perform significantly 
# faster on average than women, or vice versa.
# Collapse the data again, using 
# cast(data, var1 + var2 + var3 ... ~ ., function, value = var4, na.rm = T)

datcast2<-dcast(dat, Gender ~., mean, value.var = "StimulDS1.RT", na.rm = T)
datcast2

# Take a look at the resulting data frame using head()
head(datcast2)

# Compute the t-test to compare the accuracy means of female and male 
# participants.
# Which t-test do you need and why? How do you interpret the result?
help("t.test")
t.test(datcast2$.)
# One member of this gender group cannot be a member of other group as they are independent.
## So one-sample t-test would be suitable for us to calculate the result.
## As the result of p-value(0.04395) is significant(< 0.05), we will reject the null hypothesis. 
## It means that true mean is not equal to 0.

