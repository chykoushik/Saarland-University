### Stats with R Exercise sheet 6

##########################
#Week 7: Correlation and Regression
##########################


library(reshape)
library(languageR)

#######################
### PART 1: Correlation
#######################

########
### Please, use ggplot to make plots in all exercises below!
########

# Get some data - access the ratings data set in languageR and name it "data".
# Subjective frequency ratings and their length averaged over subjects, for 81 concrete English nouns.

data<-ratings
data
# Take a look at the data frame.
str(data)


# Let's say you're interested in whether there is a linear relationship between the word frequency of 
# the 81 nouns and their length.
# Take look at the relationship between the frequency and word length data by means a of a scatterplot 
# (from ggplot library).
library(ggplot2)
ggplot(data,aes(x= Length , y= Frequency )) + 
  geom_point() 


# Judging from the graphs, do you think that word frequency and word length are in any way correlated 
# with one another?

## It's hard to be sure by simply looking at the graph but seems like there might be
## a linear relationship between Length and Frequency and negatively correlated.

# Compute the Pearson correlation coefficient between the two variables by means of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variable divided by the product 
# of their variance. It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).
help("cor")
cor(data$Length,data$Frequency) # Pearson Correlation coefficient between Length and Frequency

cor(data$Length, data$Frequency,use="pairwise.complete.obs")

# Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?

# The correlation coefficient:  -0.4281462 suggests a Medium effect and 
# the direction of the effect is negative.

# Note that we have a large number of tied ranks in word length data (since there are multiple words 
# with the length of, e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to Kendall's tau instead of 
# Pearson (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?

cor(data$Length, data$Frequency,method = "kendall",use="pairwise.complete.obs")

# The correlation coefficient  -0.316297 suggest a Weak effect for Kendall's tau.
# The correlation coefficient  -0.4281462 suggest a Moderate effect for Pearson Correlation Coeffeicient.

# What about significance? Use the more user-friendly cor.test!

# As it is quite impossible to know if the result is significant or not using cor(), we will use
# cor.test() now to get the response in details.
help("cor.test")
cor.test(data$Length,data$Frequency,method = "pearson")
# The p value of pearson test is 6.685e-05. At 0.05 significance level, 
# this value is significant (6.685e-05 < 0.05). So, we may conclude that we can reject the null hypothesis.

cor.test(data$Length, data$Frequency,method = "kendall")
# The p value of Kendall tau test coming  8.907e-05. At 0.05 significance level, 
# this value is significant (8.907e-05 < 0.05). So, we may conclude that we can reject the null hypothesis.

# Take a look at the output and describe what's in there.
# What do you conclude?

# The results of both of the Pearson correlation(-0.4281462) and Kendall's tau(-0.316297) are negative.
# Hence, we may conclude that the samples that we are dealing with are not normally distributed.
# Moreover, they are negatively skewed.

# Finally, we can also calculate Spearman's rank correlation for the same data.
cor(rank(data$Length), rank(data$Frequency))

###################################################################################################


#######################
### PART 2: Regression
#######################

# Fit a linear regression model to the data frame for the variables frequency (outcome variable) 
# and Length (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"

help("lm")
linear_model1 <- lm(data$Frequency ~ data$Length, data, na.action ="na.omit")
linear_model1

# How do you interpret the output? Is the relationship between the two variables positive or negative?
# Plot the data points and the regression line.

# The output of the linear regression model shows the values of the coeffecients of the model. 
# Here, intercept, b0 = 6.5015 and slope, b1 = -0.2943

## For one unit of increasing in Length, Frequency will decrease by 29.43%. So, the relationship between two 
## variables is negative.

ggplot(data = data, aes(Length, Frequency)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

# Run the plotting command again and have R display the actual words that belong to each point. 
# (Don't worry about readability of overlapping words.)

ggplot(data = data, aes(Length, Frequency)) + geom_text(aes(label=data$Word)) + geom_smooth(method = "lm", se = FALSE)


###################################################################################################


# Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv
# You can download this data frame from material of week 6: T-tests

setwd("F:/Saarland_University/Winter_2018/Advanced_Statistics_with_R/Assignments/Assignment_6/New_folder/")
getwd()
data2 <- read.csv("digsym_clean.csv")
data2

# Suppose you want to predict reaction times in the digit symbol task by people's age.
# Fit a linear regression model to the data frame for the variables correct_RT_2.5sd (outcome variable) 
# and Age (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"

Linear_model2 <- lm(correct_RT_2.5sd ~ Age, data2, na.action = "na.omit")
Linear_model2

# Let's cast the data to compute an RT mean (use correct_RT_2.5sd) for each subject, so that we have only one Age 
# observation by Subject.
# In case you're wondering why we still have to do this - like the t-test, linear regression assumes 
# independence of observations.
# In other words, one row should correspond to one subject or item only.

library(reshape2)
cast1<-dcast(data2, Subject+Age ~., mean, value.var = "correct_RT_2.5sd", na.rm = T)
colnames(cast1) <-c("Subject","Age","RT_mean")
cast1

# Fit the regression model.

Linear_model3 <- lm(RT_mean ~ Age,  cast1, na.action = "na.omit")
Linear_model3

# Let's go over the output - what's in there?
# How do you interpret the output?

# The output of the linear regression model shows the values of the coefficients of the model
# Here, intercept is 637.93, slope is 21.22.

# Again plot the data points and the regression line. 

lm3_plot <- ggplot(cast1,aes(x= Age, y= RT_mean ))+ 
  geom_point() + geom_smooth(method="lm",se=FALSE)
lm3_plot


# Plot a histogram and qq-plot of the residuals. Does their distribution look like the normal distribution?
help("residuals")
residuals(Linear_model3)

## Histogram:
hist_plot <- ggplot(data=cast1, aes(residuals(Linear_model3))) + geom_histogram()
hist_plot

## QQ-Plot:
ggplot(cast1, aes(sample = residuals(Linear_model3)))+stat_qq()
## Yes the distribution looks like normally distributed.

# Plot Cooks distance which estimates the residuals (i.e. distance between actual values and the 
# regression line) for individual data points in the model.

?cooks.distance
cooks.distance(Linear_model3)

# It actually looks like we have 1 influential observation in there that has potential to distort 
# (and pull up) our regression line.
# The last observation (row 37) in cast yielded a Cooks D is very high (greater than 0.6).
# In other words, the of the entire regression function would change by more than 0.6 when this 
# particular case would be deleted.

# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to each point.

# The problem could be that the last observation data is distant from other observations.
# Plot will give us a clear idea about it.

## Histogram:
hist_plot <- ggplot(data=cast1, aes(cooks.distance(Linear_model3))) + geom_histogram()
hist_plot

## QQ-Plot:
ggplot(cast1, aes(sample = cooks.distance(Linear_model3)))+stat_qq()

# By looking at the plots, it is confirmed that the last observation(row 37) is an outlier. 
# As a result, it is showing a very high cooks D value.

# Make a subset of "cast" by excluding this subject and name it cast2.
cast2 <-cast1[-37,]
cast2

# Fit the model again, using cast2, and take a good look at the output.

Linear_model4 <- lm(RT_mean ~ Age,  cast2, na.action = "na.omit")
Linear_model4
Linear_model3

# What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?

# As the output of the linear regresion model shows the values of the coefficients of the model,
# we can see the intercept is 862.05 (> intercept of Linear_model3) , slope is 11.98(< slope of Linear_model3)
# As there is no outliers in the model and the slope seems a better fit than before, there is a good
# probability of age being the predictive of RTs.

# Plot the regression line again - notice the difference in slope in comparison to our earlier model fit?
library(ggplot2)
lm4_plot <- ggplot(cast2,aes(x= Age , y= RT_mean )) + 
  geom_point() +geom_smooth(method = "lm", se=FALSE)

lm4_plot
# Display the two plots side by side to better see what's going on.

install.packages("ggpubr")
library(ggpubr)
theme_set(theme_pubr())

figure <- ggarrange(lm3_plot, lm4_plot,
                    labels = c("Linear Model of cast1", "Linear Model of cast2"),
                    ncol = 2, nrow = 1)
figure

# Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Refer to Navarro (Chapter on regression) if you have trouble doing this.

Linear_model4

X <- cast2$Age
Y <- cast2$RT_mean

Y.pred <- 862.05 + (11.98*X)
SS.res <- sum((Y - Y.pred)^2)
SS.res

SS.tot <- sum((Y - mean(Y))^2)
SS.tot

R.squared <- 1 - (SS.res/ SS.tot)
R.squared

# How do you interpret this number?
#This value is the coefficient of determination which determines predictor(age) is 3.49 percent of the variance in the outcome(RT_mean)
# We can conclude that as this value is very less , so our model don't fits best to the data.


