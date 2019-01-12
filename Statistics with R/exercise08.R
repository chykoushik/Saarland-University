### Stats with R Exercise sheet 8

##########################
#Week9: Checking Assumptions underlying ANOVA and linear regression
##########################


########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########


# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1= high school education, 0= no high school degree.


data <- read.table("kidiq.txt")
summary(data)              


# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.
library(ggplot2)

ggplot(data, aes(x = mom_iq, y = kid_score, title)) + geom_point() + geom_smooth(method = "lm")+
  ggtitle("Kid's Score vs Mom's IQ") + xlab("Mom's IQ") + ylab("Kid's IQ")



# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.

linear_model1<-lm(kid_score~ mom_hs, data)
linear_model1

#For intercept:
# If mom_hs has no high school degree, then the kid_score would be 77.55

#For slope:
# If mom_hs increases one unit then the kid_score will be increased by 11.77 times.


# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.

linear_model2<-lm(kid_score ~ mom_hs + mom_iq, data)
linear_model2

#For intercept:
# If mom_hs has no high school degree and mom_iq = 0, then the kid_score would be 25.7315

#For slope:
# If mom_hs increases one unit then the kid_score will be increased by 5.9501
# If mom_iq increases one unit then the kid_score will be increased by 0.5939

#Comparing two models:
# Looking in to two models we may conclude that mom_hs as a single predictor has more influence on kid_score
# than combining mom_hs and mom_iq together.

# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without one in another color. Then also fit two separate regression lines such 
#    that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(your_model))
predict(linear_model2)

ggplot(data = data, aes(x= mom_iq, y= kid_score, color = factor(mom_hs))) + geom_point() +
  scale_color_manual(guide=guide_legend(title="mom_hs"), values=c("blue","red"))+
  geom_abline(intercept= linear_model2$coefficients[1], slope= linear_model2$coefficients[3], color="blue")+
  geom_abline(intercept= linear_model2$coefficients[1] + linear_model2$coefficients[2], slope= linear_model2$coefficients[3], color= "red") +
  ggtitle("Kid's Score vs Mom's IQ") +
  xlab("Mom's IQ") + ylab("Kid's Score")


# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Interpret your results.


linear_model3<-lm(kid_score ~ mom_hs * mom_iq, data)
linear_model3

# For Intercept:
# When mom_hs and mom_iq has no influence, kid_score will be decreased by 11.4820.

# For Slope:
# For one unit increase in mom_hs, kid_score will be increased by 51.2682.
# For one unit increase in mom_iq, kid_score will be increased by 0.9689.
# The effect of mom_hs on kid_score decrease by 0.4843 for every unit increase in mom_iq

# g) Next, let's plot the results of this model.

ggplot(data,aes(y=kid_score, x = mom_iq, color=factor(mom_hs)))+geom_point()+ geom_smooth(method="lm",se=FALSE)

# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.

new <- data.frame(mom_iq=100, mom_hs=1)
new
?predict.lm
pred_int<-predict(linear_model3, new , interval = "confidence", level=0.95)
pred_int
# i) Meaning of confidence intervals for regression line.
#    Let's go back to the exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of the confidence interval?
library(ggplot2)

ggplot(data, aes(x = mom_iq, y = kid_score, title)) + geom_point() + geom_smooth(method = "lm")+
    ggtitle("Kid's Score vs Mom's IQ") + xlab("Mom's IQ") + ylab("Kid's IQ") 
# the gray shade in the regression model depicts confidence interval of 0.95. it tells the probability of the model of kid_score with 
# mom_iq which lie within confidence interval of the fitted model. The 95 percent confidence interval of mean kid_score
# for the mom's iq  lies between 86.31365 and 90.18167


# j) Finally, do model checking on your model with the interaction, i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.
#install.packages("ggfortify")

library(ggfortify)
ggplot2::autoplot(linear_model3, which = 1:6, ncol = 3, label.size = 3)
# for linearity plot 1
# The blue line in the plot is almost horizontal to zero axis, this implies there is no pattern for residuals. So our model is linear.

# for normality plot 2
# here from the plot we can see the most of the points lie on straight line, so we can assume that plot is normal

# Homogeneity 3 
#For this property the blue line should be horizontal which means the residuals are spread equally. In this case the variance of residual points 
# are decreasing wrt the fitted outcome variable.

# cook's distance plot 4 
# values 111 , 281 , 213 are highly influential in the data as these values are having more cook's distance.

# residuals vs leverage plot 5 
#the plot describes three extreme points 281, 213 , 111.

# cook's distance vs leverage plot 6
#Three points which appear to be influential and all other points lie towards left together. 



