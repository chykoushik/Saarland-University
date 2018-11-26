### Stats with R Exercise sheet 4

##########################
#Week5: Tests for Categorial Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Koushik Chowdhury
## Matriculation number: 2572865


##Group Members: 
##Name: Ashima Jindal
## Matriculation No: 2573144
## Name: Rayhanul Islam Rumel
## Matriculation No:2576541  

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

#################################################################################
#################################################################################

##########
##Exercise 1. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 4 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.

dbinom(4, 12, 1/5)

## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance. 
sum(dbinom(c(0,1,2,3,4), 12, 1/5))

##########
##Exercise 2. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from our first tutorial again. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?
library(languageR)
summary(dutchSpeakersDistMeta)
class(dutchSpeakersDistMeta$Speaker)
class(dutchSpeakersDistMeta$Sex)
class(dutchSpeakersDistMeta$AgeYear)
class(dutchSpeakersDistMeta$AgeGroup)
class(dutchSpeakersDistMeta$ConversationType)
class(dutchSpeakersDistMeta$EduLevel)

# Except AgeYear each variable (Speaker, Sex, AgeGroup, ConversationType, EduLevel) is factor.

## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.

 table(dutchSpeakersDistMeta$AgeGroup)

age<- table(dutchSpeakersDistMeta$Sex, dutchSpeakersDistMeta$AgeGroup)
age
##    Visualize your data with a single bar plot (use ggplot) that represents the counts with respect to each age group 
##	  and each sex.

library(ggplot2)

ggplot(dutchSpeakersDistMeta, aes(x= dutchSpeakersDistMeta$Sex, y = dutchSpeakersDistMeta$AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge")

## c) Inspect the table 'age' you created. Does it look like there could be a significant 
##    difference between the sexes?

summary(age)
##  After looking in to the  p-value (0.5124), it is clear that the value is greater than 0.05
##  Hence, we can conclude by saying that there is no significant difference in between the sexes.


## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group
##    using the function chisq.test. Look at the help of this function. Then use the 
##    function to calculate whether there's a difference in our table 'age'. 
##    Is there a significant difference in age group?

help(chisq.test)
chisq.test(age)
##  we have compared the chisq value (= 3.278) with the value we get 
##  from the table of critical values of chisq, 9.488 (Degree of Freedom = 4, Significance level = 5%).
##  It is clearly observable that chisq value is less than the critical value. 
##  Hence, there is no significant difference in age group.

## e) What are the degrees of freedom for our data? How are they derived?

## The degree of freedom for our data is 4. The degree of freedom refers to the number of values that are free to vary. 
## It is usually derived as (r-1)(c-1) where r is no of rows and c is no of coloumns. 
## In our case, r=5 , c=2. Therefore, df=(5-1)(2-1) = 4

##########
##Exercise 3. Binomial versus chi-square
##########
##    In this exercise, we'll do significance tests for a paper on therapeutic touch 
##    (google it if you want to know what that is...) that was published in the Journal 
##    of the American Medical Association (Rosa et al., 1996).
##    The experimenters investigated whether therapeutic touch is real by using the 
##    following method:
##    21 practitioners of therapeutic touch were blindfolded. The experimenter 
##    placed her hand over one of their hands. If therapeutic touch is a real 
##    phenomenon, the principles behind it suggest that the participant should 
##    be able to identify which of their hands is below the experimenter's hand. 
##    There were a total of 280 trials, of which the therapeutic touch therapists 
##    correctly indicated when a hand was placed over one of their hands 123 times.

## a) What is the null hypothesis, i.e. how often would we expect the participants to 
##    be correct by chance (in raw number and in percentage)?

#According to R.A Fisher, Null hypothesis is tested for possible rejection under the assumptiopn it is true.
#Null hypothesis explains there is no statistical relationship or association exists in given observation. 
#In Null hypothesis, sample observation result due to chance.
#123 times out of 280 trials are correct by chance. Percentage would be 44% approx and raw number is 0.4392857.
binom.test(123,280,0.5)

## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 
observed = c(123, 157)
expected = c(0.50, 0.50)

chisq.test(x = observed,
           p = expected)
## As the chi squared value (4.128) is greater than the critical value (3.841) from the table of critical values
## under the degree of freedom: 1 and Significance level: 5%, we can conclude that therapeutic touch works 
## and we can reject the null hypothesis.

## c) Now calculate significance using the binomial test as we used it in exercise 1.

## for significance we will calculate  the z value
mu<-280*0.5
mu ##mean
vari<-280*0.5*0.5
vari ##variance
sd=sqrt(vari)
sd ##standard dev
z<-(123-mu)/sd
pnorm(z) ## this is for one tailed test
2 * pnorm(z) ## this is for two tailed test as this value is less than 0.05 so we will reject the null hypothesis
## So our result is significant

## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?

##  The results are slighly differed for these two tests but we will prefer chi square over other as it can be used for more than two categorical variables.
##  As  for our case hand location is the categorical variable which has more than two values (left, right, middle).

##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

##  for example you have been hired to work for the pizza hut and part of your job is to find how pizza advertisements are doing.
##  So for a sample of N=100 and we will ask them to see ads. Before viewing any ads will they choose to order pizza  and then after
##  after viewing the ads if anyone of them has changed their minds. we will describe data in contigency table. we will see that we have 100 participants 
##  but 200 observations. this is because each person has anwswered yes or no. it could be possible that first person will say no first time after that
## same person can say yes after viewing ads. The consequence of this is that the unormal chi square will not give correct results due to 
## violation of independence assumption.In this situation we will use McNemar's test.




