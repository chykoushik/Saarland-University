### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################

###########################################################################################



#######################
### PART 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, Cambridgeshire
# County Council considered 14 pairs of locations. The locations were paired to account 
# for factors such as traffic, volume and type of road. One site in each pair had a sign 
# erected warning of the dangers of speeding and asking drivers to slow down. No action 
# was taken at the second site. Three sets of measurements were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the erection 
# of the sign, shortly after the erection of the sign, and again after the sign had been 
# in place for some time.

# 1. For the further reference please use ?amis. It may take some time to understand the dataset. 

?amis

# 2. Load the dataset and briefly inspect it. 
# Feel free to make some plots, calculate some statistics in order to understand the data.
amis                         #LOAD DATASET
summary(amis)
str(amis)

hist(amis$speed)             #HISTOGRAMS
dotchart(amis$speed)         #DOT PLOT
mean(amis$speed)             #MEAN
median(amis$speed)           #MEDIAN
lm(amis$speed ~ amis$period) #LINEAR REGRESSION


# 3. All our columns have numeric type. Convert the categorial columns to factors.
str(amis)
t1=Sys.time()
for(i in 1:ncol(amis)) amis[,i]=as.factor(amis[,i])
Sys.time()-t1
str(amis)
# 4. Build a boxplot for the distribution of `speed` for each of `period` values 
# (before, immediately after and after some time). Build 2 plots side by side
# depending on the `warning` variable.
# (for all plots here and below please use ggplot)

 ggplot(amis, aes(x = period, y = speed)) +
  geom_boxplot(aes(group = period)) +  facet_grid(. ~ warning)

# 5. What can you conclude according this plot? What can you say about people behaviour in
# different periods: before, immediately after and after some time?
 
# By looking at the plot we can easily see more outliers of speed in warning 2 where there were no signs
 # for peiod before , in warning 1 drivers start to increase their speed in range 53-58 and same can be observed in warning 2
# for imediatelly after period the drivers reduced their speed for warning 1 but in warning 2 drivers are exceeding their speed.
 # for after sometime in warning 1 , drivers reduced their speed and same can be seen in warning 2 
 

# 6. What are your ideas about why the data with warning==2 (which correspond to the
# measurements in different times on sites where no sign was erected) was collected?
 
 # this is because to check the variation in speed of the drivers according to different times with no sign. 
  # we can easily see from the warning 2 plot , people with period before mosly having a large speed and  immediately after  this speed increases
 # , and for period after sometime , the speed of the drivers reduced.


#######################
### PART 2: 1-way ANOVA
#######################

#1. First let's create a new data frame which will be used for all PART 2.
# For 1-way ANOVA we will be working with the subset of `amis` where the 
# warning sign was erected, which corresponds to warning==1, therefore first
# subset your data to filter out warning==2 and then apply cast() to average
# speed over each "pair" and "period. Assign this new data frame to the variable casted_data.

 # Converting factors to categorial columns
 str(amis)
 t1=Sys.time()
 for(i in 1:ncol(amis)) amis[,i]=as.numeric(amis[,i])
 Sys.time()-t1
 str(amis)
 
anov_data <- subset(amis,amis$warning == 1)
anov_data



library(reshape2)

casted_data<-dcast(anov_data, pair+period ~., mean, value.var = "speed", na.rm = T)
colnames(casted_data) <-c("pair","period","speed_mean")
casted_data

# 2. Build a boxplot of the average speed depending on period

ggplot(casted_data, aes(x = period, y = speed_mean)) +
  geom_boxplot(aes(group = period)) 

# 3. Is there a difference between the periods?

# the speed in period 1 (before) is less for warning 1 , in period 2 (immediate after) the speed of drivers increased 
# in period 3 (after sometime) the speed of the drivers reduced coming to the lower range.

# 4. Now, let's check each ANOVA assumptions and whether they are violated or not and why.

anova1<-aov(speed_mean ~ period, data = casted_data)
anova1
# a) Independence assumption
# (you need to figure out the best way to do it and give a detailed justified answer)

#for the independence assumption, we can do t test.T-test  
#can be treated as a particular case of one-way ANOVA for independence assumption.
#for dependent variable, independent t test distributes within each group.

t.test(casted_data$speed_mean, casted_data$period)

# The p-value (2.2e-16) is smaller than 0.05. Hence, we can reject the null hypothesis.
# So the true difference in means is not equal to 0. It means the means are independent.

# b) Normality of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)

# for the normality of the variables we can perform shapiro test
shapiro.test(residuals(anova1))
# The p value is 0.1798 which is greater than the signficance level of 0.05, therefore we can't reject the null hypothesis.
# this implies that data has been taken from normal populations.

# c) Homogeneity of variance of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)
library(car)
#levene test can be used to check homogeneity

leveneTest(casted_data$speed_mean, casted_data$period, center = mean)

# We chose mean instead of median as the center as it gives us the better result. 
# By looking at the Pr-value (0.635 > 0.05), we can interpret that the result is not significant.
# Hence, we may conclude that the assumption of homogeneity of variance of residuals 
# doesn't hold in this context.

# 5.Now we are ready to perform 1-way ANOVA: please use the function aov() on the speed
# depending on the period,report p-value and interpret the result in details
?aov

anova1<-aov(speed_mean ~ period, data = casted_data)
anova1
summary(anova1)
#The output includes the column F value and Pr value which corresponds to the p value of the test which is 0.405.
# p value 0.405 is greater than the significance level of 0.05 which implies that there are not significant differences between the groups 

# 6. Please do a pairwise t-test with pairwise.t.test()
?pairwise.t.test
no_adjust<-pairwise.t.test(casted_data$speed_mean, casted_data$period,  p.adjust.method = "none")
no_adjust
# 7. Report pair-wise p-values and interpret the result in details
# this test is giving different p values and all of them are not sifnificant as those values are greater than 0.05 significance level

# 8. Try to use no adjustment for pairwise testing and then Bonferroni correction.
# Does the result change?
# using Bonferroni correction, 
bh<-pairwise.t.test(casted_data$speed_mean, casted_data$period, p.adjust.method = "BH")
bh
# yes the result gets changed, one of the value gets three times and other one values gets increased


#######################
### PART 3: 2-way ANOVA
#######################
# 1. Now we want to analyze the influence of 2 categorial variables (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1)
# First, we need to again average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2

# Converting factors to categorial columns
str(amis)
t1=Sys.time()
for(i in 1:ncol(amis)) amis[,i]=as.numeric(amis[,i])
Sys.time()-t1
str(amis)

library(reshape2)

casted_data2<- dcast(amis, warning + period ~ . , mean, value.var =  'speed')
casted_data2
colnames(casted_data2) <-c("warning","period","avg_speed")
casted_data2
summary(casted_data2)

# 2. Calculate the mean for each of 6 pairs of `period` and `warning`
library(plyr)
library(dplyr)

mean_anov<-group_by(casted_data2, period, warning) %>%
  summarise(
    count = n(),
    mean = mean(avg_speed, na.rm = TRUE),
    
  )
mean_anov

# 3. Do you think there is a significant difference in some of the groups?
# There is no significant difference in the groups

# 4. Now apply 2-way ANOVA: please use the function aov() on the speed depending on the period and warning
# report p-value and interpret the result in details

anova_model<- aov(avg_speed ~ period * warning, casted_data2) 
anova_model
summary(anova_model)
# the value of p for period is 0.2526 which is greater than 0.05 significance level.
# This implies that speed is not significantly affected by period.
# The value of p for warning is 0.0671 which is less than than 0.1 significance level that
# means speed is  significantly affected by warning.


# 5. What do you conclude about the behaviour of drivers based on the 2-way ANOVA?

# The p value for interaction is 0.9246 which is greater than significance level 0.05 , so this implies that there is  no significant interaction
# of period and warning with speed of vehicles. But the speed of vehicles is significanly affected by warning.
