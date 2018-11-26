### Stats with R Exercise Sheet 3

##########################
#Week4: Hypothesis Testing
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 11. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 


## Please write below your (and your teammates) name, matriculation number. 
## Name: Koushik Chowdhury
## Matriculation No: 2572865

##Group Members: 
##Name: Ashima Jindal
## Matriculation No: 2573144
## Name: Rayhanul Islam Rumel
## Matriculation number: 2576541

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)


###############
### Exercise 1: Deriving sampling distributions
###############
## In this exercise, we're going to derive 5 sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.

library(languageR)
help("dative")
summary(dative)

## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?

table(dative$LengthOfTheme) 
# table is performing the tabulation of data as well as the frequency of each values (mode).


## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?

hist(dative$LengthOfTheme)
boxplot(dative$LengthOfTheme)

# yes, there is  an outlier in boxplot. By viewing the histogram, data is positively skewed right side.

## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?

# In Distribution, where every member of some data is plotted against with another variable.
# In this the mean is not closer that much to our actual one where in Sampling distribution, 
# we will randomly choose the sample size and plot the graph. 
# In this more the sample size, more closer we will be towards actual mean.

## e) We are going to need a random sample of the variable 'LengthOfTime'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'

randomsampleoflengths <- sample(dative$LengthOfTheme, 5)
randomsampleoflengths
## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 

randomsampleoflengths2 <- sample(dative$LengthOfTheme, 5)
randomsampleoflengths2

## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.

mean(randomsampleoflengths)
mean(randomsampleoflengths2)

means5 <- (c(mean(randomsampleoflengths), mean(randomsampleoflengths2)))
means5


## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.


for(i in 1:1000) { 
  means5[i] <-  mean(sample(dative$LengthOfTheme, 1000))
  print(means5[i])
}




## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.


for(i in 1:1000) { 
means50[i] <-  mean(sample(dative$LengthOfTheme, 50))
print(means50[i])
}


## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?

## means5 contains the random sample 1000 values and means50 contains the random sample of 50 values.
## Due to change of the length of sample from 1000 to 50, we are getting inconsistence average.
## So, in order to get a mean closer to actual one, we should take the sample size of 1000.

## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 has a positive or negative skew?

hist(means5, breaks = 15)
hist(means50, breaks = 15)
# means5 has a negative skew.



## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?

# because the sample size of means5 is bigger than means50 that is 1000 and 
# eventually frequency of each value occuring in means5 is more.

### Exercise 2: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?

## The confidence interval only mean a probability estimate for an experiment replication.

## b) Let's calculate the confidence interval for our means from the previous 
##    question.
##    First, install and load the packages 'lsr' and 'sciplot'
install.packages("lsr")
install.packages("sciplot")

library("lsr")
library("sciplot")

## c) Look at the description of the function ciMean to see which arguments it takes.
help(ciMean)

## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the means for the variable LengthOfTheme.
ciMean(dative,conf = 0.95)
mean(dative$LengthOfTheme)

## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?
help(qnorm)
qnorm(p=c(.025,.975))
## yes, as 95 percent of all the distribution falls within 1.96 SD on either side of mean 

## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.
help(bargraph.CI)
bargraph.CI(x.factor = AnimacyOfTheme, response = LengthOfTheme , data = dative) 

## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?

bargraph.CI(x.factor = AnimacyOfTheme, response = LengthOfTheme , data = dative, ci.fun=ciMean) 
## A we are using ciMean as the  ci.fun parameter which give  95 percent confidence interval
# it means we are getting more values within the confidence interval

###############
### Exercise 3: Plotting graphs using ggplot.
###############
# There are many ways of making graphs in R, and each has its own advantages 
# and disadvantages. One popular package for making plots is known as ggplot2. 
# The graphs produced with ggplot2 look professional and the code is quite easy 
# to manipulate.
# In this exercise, we'll plot a few graphs with ggplot2 to show its functionalities.
# You'll find all the information you'll need about plotting with ggplot2 here: 
# http://www.cookbook-r.com/Graphs/
# Also, you have been assigned the ggplot2 course in DataCamp. Please work through 
# this course (Please, set up your name in the datacamp profile. 
# So I can find you quickly.)

## a) First install and load the ggplot2 package. Look at the help for ggplot.
install.packages("ggplot2")
library("ggplot2")
help("ggplot")

## b) We're going to be plotting data from the dataframe 'ratings' 
##    (included in languageR). 
##    Look at the description of the dataset and the summary.
library("languageR")
library("dplyr")

data(ratings)    # To load the dataset
glimpse(ratings) # To check the observations and variables. Also, the datatype of the variables.
head(ratings)    # To view how the dataset has been organized.
summary(ratings) # To view the overall summary of the variables.

## For each word, we have three ratings (averaged over subjects), one for the 
## weight of the word's referent, one for its size, and one for the words' 
## subjective familiarity. Class is a factor specifying whether the word's 
## referent is an animal or a plant. 
## Furthermore, we have variables specifying various linguistic properties, 
## such a word's frequency, its length in letters, the number of synsets 
## (synonym sets) in which it is listed in WordNet [Miller, 1990], its 
## morphological family size (the number of complex words in which 
## the word occurs as a constituent), and its derivational entropy (an 
## information theoretic variant of the family size measure). 
## Don't worry, you don't have to know what all this means yet in order to 
## be able to plot it in this exercise!

## c) Let's look at the relationship between the class of words and the length. 
##    In order to plot this, we need a dataframe with the means.
##    Below you'll find the code to create a new dataframe based on the existing 
##    dataset ratings.
##    Plot a barplot of ratings.2 using ggplot. Map the two classes to two 
##    different colours. 
##    Remove the legend.
summary(ratings)
condition <- c("animal", "plant")
frequency <- c(mean(subset(ratings, Class == "animal")$Frequency), mean(subset(ratings, Class == "plant")$Frequency))
length <- c(mean(subset(ratings, Class == "animal")$Length), mean(subset(ratings, Class == "plant")$Length))
ratings.2 <- data.frame(condition, frequency, length)
ratings.2


ggplot(ratings.2, aes(x = length, y = frequency, fill = condition)) +
  geom_bar(stat = "identity", show.legend = FALSE)

## d) Let's assume that we have additional data on the ratings of words. 
##    This data divides the conditions up into exotic and common animals 
##    and plants.
##    Below you'll find the code to update the dataframe with this additional data.
##    Draw a line graph with multiple lines to show the relationship between 
##    the frequency of the animals and plants and their occurrence.
##    Map occurrence to different point shapes and increase the size 
##    of these point shapes.
condition <- c("animal", "plant")
frequency <- c(7.4328978, 3.5864538)
length <- c(5.15678625, 7.81536584)
ratings.add <- data.frame(condition, frequency, length)
ratings.3 <- rbind(ratings.2, ratings.add)
occurrence <- c("common", "common", "exotic", "exotic")
ratings.3 <- cbind(ratings.3, occurrence)
ratings.3

ggplot(data=ratings.3, aes(x=occurrence, y=frequency, group=3, color = condition, shape = occurrence)) +
  geom_line()+
  geom_point()

ggplot(data=ratings.3, aes(x=occurrence, y=frequency, group=3, color = condition, shape = occurrence)) +
  geom_line()+
  geom_point(size = 5)

## e) Based on the graph you produced in question d, 
##    what can you conclude about how frequently 
##    people talk about plants versus animals, 
##    with regards to how common they are?

## In perspective common occurences, it is easily oberservable that people talk about animal more 
## frequently (of about 5.13%) than plants (of about 4.27%)  
