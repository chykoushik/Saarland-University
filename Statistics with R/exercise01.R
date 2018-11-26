### Stats with R Exercise sheet 1 


###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()

## b) Get help with this function.
help(getwd)

## c) Change your working directory to another directory.

setwd("C:/users/sony/Desktop/uni saarland/uds/R") # 1(c)



###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages(languageR) 

## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?
dutchSpeakersDistMeta 
head(dutchSpeakersDistMeta) # head is showing some of the first observations
tail(dutchSpeakersDistMeta)  # tail is showing some of the last observations 
summary(dutchSpeakersDistMeta)  # summary is showing quick  review of dataset


## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.
nrow((dutchSpeakersDistMeta)  )

## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.

boxplot(dutchSpeakersDistMeta[2:3])

## e) Does it seem as if either of the two groups has more variability in age?
#2(e) yes,as the spread of age plot is more

## f) Do you see any outliers in either of the two groups? 
#2(f) no

## g) Now calculate the mean and standard deviation of the AgeYear per group.  
##    Do this by creating a subset for each group. 
##    Do the groups seem to differ much in age? 

f<-subset(dutchSpeakersDistMeta,Sex=="female")
m<-subset(dutchSpeakersDistMeta,Sex=="male")
mean(m$AgeYear)# Mean of male group
sd(m$AgeYear) # SD of male group
mean(f$AgeYear) #  Mean of female group
sd(f$AgeYear) # SD of female group
#No the groups don't differ much in age 

## h) What do the whiskers of a boxplot mean?
#whiskers will tell how the data  is extended to minimum and maximum values in the set which is equal to 1.5 times IQR

###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)

##   Answer: This data is discrete and measurement scale is ordinal scale because it is the total number of a value ('and then')
##   resulted from 25 children and we can compare and order the given set of values.

## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?

##  Answer: 
##  After going through the questions (c-e), I would prefer a dataframe over a matrix. Because,

##  1) I will need to create vectors to add the ids of participants as well as the set of values 
##  received by the researcher. Working with vectors in dataframe is more flexible than matrix in R.
##  2) It is very user friendly to convert a vector to factor in dataframe.

## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25

## Answer:

pps <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)

## d) Next, create a vector containing all the observations. Name this vector 'obs'.

## Answer:

obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)

## e) Create a dataframe for this data. Assign this to 'stories'. 

## Answer:

stories <- data.frame(pps, obs)
str(stories)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?

## Answer:

## Summary of the dataframe:

summary(stories)

## Class of pps:

class(stories$pps)

## Class of obs:

class(stories$obs)

## 'pps' is the variable of numeric class.

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?

## Answer:

fact.pps <- factor(stories$pps)
fact.pps

## Factor is better:

## As we will use the data in few of the plottings, it is better to be used as factor. 
## It is well known to us that factors can easily be used and implemented correctly in statistical modeling.
## Also, to store string variables as factor variables is considered as more efficient way of using memory.

## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.

## Answer:

## Histogram for 'pps':

hist(stories$pps, breaks = 8, main="Histogram of PPS",
     xlab="PPS")

## Histogram for 'obs':

hist(stories$obs, breaks = 8, main="Histogram of OBS",
     xlab="OBS")

## i) Create a kernel density plot using density().

## Answer

## Kerner Density Plot of 'pps':
pps.kdp <- plot(density(stories$pps))

## Kerner Density Plot of 'obs':
obs.kdp <- plot(density(stories$obs))


## j) What is the difference between a histogram and a kernel density plot?

## Answer:

##  The difference between Histogram and Kernel Density Plot:

##   Histograms are being constructed by binning data. We can only choose histogram for numerical data. 
##  It is easy to deal with large data sets in histogram as they can be easily grouped within intervals. 
##  However, as same data can be presented in different manner, the resulting estimate of histogram is not 
##  always smooth.


##   Kernel Density Plot, a non-parametric model and considered as the more modern version of histogram, 
##  is a presentation of a smooth curve graph. It is a more effective way to view the distribution of 
##  random variable(s). The resulting estimate of KDP is more detailed than histogram.


## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)

## Answer:

## Overlay the histogram with the kernel density plot for 'pps':

hist(stories$pps, prob=TRUE)
lines(density(stories$pps))







###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.

x <- seq(-5, 5, by = 0.1)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.

help(dnorm)
y <- dnorm(x, mean = 0, sd = 1, log = FALSE)

## c) Now use plot() to plot the normal distribution for z values of "x". 

plot(x,y)

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.

y <- seq(0, 0.8, by = 0.1) 

plot(y)

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.

help(abline)
abline(v=x)
mx <- median(x, na.rm = FALSE)
abline(v =mx,lty=2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".

beaver1

b1temp <- beaver1[3]

b1temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.

mean(b1temp[,])

sd(b1temp[,])

nd <- dnorm(b1temp[,], mean(b1temp[,]), sd(b1temp[,]), log = FALSE)

plot(b1temp[,], nd)

## h) We observe two tempareatures (36.91 and 38.13). What's the likelihood that
##    these temepratures (or more extreme ones) respectively come 
## from the normal distribution from g)?

#answer: According to g plot, it is clear that 36.91 comes from the normal distribution 
#        because the temperature range of the plot is from 36.2 to 37.8. As there is 114 
#        observation, the probability to get 36.91 is 1/114. On the other hand, 38.13 don't 
#        come from the normal distribution as it is out of the range.

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histrogram based on this sample.
##    Repeat 5 times. What do you observe?


randomsample1 <- sample(b1temp[,], 20, replace=TRUE)
randomsample2 <- sample(b1temp[,], 20, replace=TRUE)
randomsample3 <- sample(b1temp[,], 20, replace=TRUE)
randomsample4 <- sample(b1temp[,], 20, replace=TRUE)
randomsample5 <- sample(b1temp[,], 20, replace=TRUE)


hist(randomsample1)
hist(randomsample2)
hist(randomsample3)
hist(randomsample4)
hist(randomsample5)

#answer: it randomly selects 20 samples from the normal distribution and plots a histrogram.
# 		 That is the reason why, histrogram are not same for the different 5 random sample.
#		 and thats why it gives different 5 histrogram.
