### Stats with R Exercise sheet 10

##########################
#Week 11: Model Selection, Transformations, Power
##########################


###########################################################################################
###########################################################################################


#Play around with the simulation code. The code simulates how a dataset may be generated. 
#The advantage over using a real data set is that we know exactly how the data was generated, 
#and can observe whether the model manages to correctly identify the original model structure and coefficients.
# IMPORTANT! Run each model simulation code several times to see how stable the results are -- this is necessary
#because we are sampling the data randomly, so it could be that we sometimes get more or less "lucky" draws.
#install.packages("lme4")
library(lme4)
library(car)

n <- 200 # number of observations to be simulated
n
predA <- rnorm(n, 100, 20)
predA
predB <- rnorm (n, 60, 30)
predB
interact <- 0.002*(predA*predB) 
interact
error <- rnorm (n, 0, 30)
error
resp <- 25 + predA + 1.2*predB - interact + error
resp
d <- data.frame(predA, predB, resp)
d
# 1. Write down what values you would hope for the model to estimate in the ideal case:
# a)intercept= 29.668619
# b)predA= 0.876429
# c)predB= 1.196705
# d)predA:predB = -0.001089

 m1<- lm(resp~predA*predB, data=d)
 m1
# Ignore the warning message about rescaling for now, we'll get to that below.
summary(m1)  
# 2. Can the model recover the original model structure and estimate correct coefficients for the predictors?
cor(predA,predB)
#output: [1] 0.04909405
#yes, the model can recover the original model structure and estimate correct coefficients for the predictors
#as the cor(predA,predB) gives 0.04909405 which is positive.


# 3. What happens if you change the number of subjects?

#if we change the subject, the model would provide worse approximations and vice versa.

# 4. What happens if you change the variance of the error term?

#The would provide better result and vice versa if we change the variance of error term.

# 5. What happens if you change the effect sizes?

# the model would start to lose linearity if there is any change in effect sizes. 

# Next, we want to observe the effect of scaling the predictors. 
# By hand, we could do: normpredA <- (predA - mean(predA)) / sd(predA)
# this is the same as calling "normpredA <- scale(predA)"
# we can do this for the whole data frame:
nd <- as.data.frame(scale(d))
nd
sm1<- lm(resp~predA*predB, data=nd)
sm1
summary(sm1)
vif(m1)
vif(sm1)

# 6. Are the predictors currently correlated? What does the vif value mean?

# a value of this  1.002284 for predA and 1.011005 for predB closer to 1  means that predictors are not correlated currently 
#but they were highly correlated earlier without normalisation because predA has value 5.881243 and predB has 33.284282. A value of vif greater than 4
# makes the predictors correlated without scaling. Vif stands for variation inflation factor. it just tells us the extent of correlation between the predictors.

# 7. Check whether normalization also has a large effect when there is no interaction present in the model

sm2<- lm(resp~predA+predB, data=nd)
m2<- lm(resp~predA+predB, data=d)
summary(sm2)
summary(m2)
vif(m2)
vif(sm2)
# A value of this 1.000973 for predA and 1.000973 for predB closer to 1  means that predictors are not correlated currently and also 
#there were not correlated earlier without scaling(predA 1.000973 , predB 1.000973 ). So normalisation doesn't effect the model as 
# the results are same for predictors before and after normalisation when there is no interaction in model.

# 8. Try out what happens if there was originally no interaction in the data.
# if there were no interaction, then we will remove this part of interaction from the data
resp1 <- 25 + predA + 1.2*predB + error
resp1
d1 <- data.frame(predA, predB, resp1)
d1
nd1 <- as.data.frame(scale(d1))
nd1

sm3<- lm(resp~predA*predB, data=nd1)
m3<- lm(resp~predA*predB, data=d1)
summary(sm3)
summary(m3)
vif(m3)
vif(sm3)
# It doesn't made any difference in the vif. The predictors are still not correlated by comparing their vif (pred A 1.002284 , pred B 1.011005) but are correlated 
# without normalisation (pred A 5.881243, predB 33.284282 ).  

#Next, we want to calculate interpretable estimates
summary(sm1)
names(sm1)

 denormPredA <- coef(sm1)[2] *sd(d$resp)/ sd(d$predA) 
 denormPredA
# value = 0.9771169
 
# 10. Explain in your own words, why the denormalization for predictor A works this way.
 
  #The above formula of denormalization can be derived from the least-squares estimate of the slope coefficient of PredA. The slope coefficient is equal to the correlation
 #times the ratio of the standard deviation of response to the standard deviation of PredA.
 
denormPredB <- coef(sm1)[3] * sd(d$resp)/ sd(d$predB)
 denormPredB
 
 #value =  1.102796
# expected: 1.2

 denormIntercept<-coef(sm1)[1] * sd(d$resp)+mean(d$resp)-
 (denormPredA*mean(d$predA) + denormPredB* mean(d$predB))
 denormIntercept
 
 #value = 21.80173
# expected: 25

denormInteract <- coefficients(sm1)[4] / (sd(d$predA)*sd(d$predB)) * sd(d$resp)
denormInteract


# Next, we create correlated variables 
n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB<- rnorm (n, 60, 30)
predC <- -1* predA + rnorm(n,0,10)
error <- rnorm (n, 0, 30)
respcor <- 25 + predA + 3* predB+ 2*predC - (0.02*(predA*predC)) + error
d2<-data.frame(predA, predB, predC, respcor)
summary(lm(respcor ~ predA * predC + predB, data=d2))

sd2 <-as.data.frame(scale(d2))
summary(lm(respcor ~ predA * predC + predB, data=sd2))

# 11. What do you observe regarding the results from the models? Do the models obtain the same or different results 
# with / without normalization?

# By looking in to the summary, we may conclude that the results are not the same before and after normaliztion.
# We might report it as below:
#Before Normalization:

#Accuracy of the fitted model:
# "RSE(195) = 30.77, p < 2.2e-16"

#Important Predictors:
# "predB" and "predC" have significant affect on "respcor".

#Interpretation of interaction terms:
# If the interaction term of predA and predC increase by one unit, then the respcor will be
# decreased by 0.019907 when other predictors are fixed.

#After Normalization:

#Accuracy of the fitted model:
# "RSE(195) = 0.2788, p < 2.2e-16"

#Important Predictors:
# "predA", "predB" and "predA:predC" have significant affects on "respcor".

#Interpretation of interaction terms:
# If the interaction term of predA and predC increase by one unit, then the respcor will be
# decreased by 0.08065 when other predictors are fixed.

# 12. Denormalize the coefficients.

#Calling an object to hold the normalized multiple linear regression model:
norm_d2 <- lm(respcor ~ predA * predC + predB, data=sd2)

#Denormalizing predA:
denorm_predA <- coef(norm_d2)[2] * sd(sd2$respcor)/ sd(sd2$predA)
denorm_predA

#Denormalizing predB:
denorm_predB <- coef(norm_d2)[3] * sd(sd2$respcor)/ sd(sd2$predB)
denorm_predB

#Denormalizing predC:
denorm_predC <- coef(norm_d2)[4] * sd(sd2$respcor)/ sd(sd2$predC)
denorm_predC

#Denormalizing Intercept:
denorm_Intercept <- coef(norm_d2)[1] * sd(sd2$respcor) + mean(sd2$respcor) -
  (denorm_predA * mean(sd2$predA) + denorm_predB * mean(sd2$predB) + denorm_predC * mean(sd2$predC))
denorm_Intercept

library(lme4)

# Finally, we will generate repeated measures!
# For this, we will use the dataframe d; for simplicity of interpretation, we will do no normalization here.
n<-400
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
error <- rnorm (n, 0, 30)
subjno <- 20 #number of subjects; 
itemno<- 20 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)
# basic data frame done; now on to by subject and by item random effects:
subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjeffdf<-data.frame(subjid, subjint)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here!
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m0<-lm(respr ~ predA + predB , data=lmerd)
summary(m0)
m1<-lmer(respr ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m1)

lmerd$resp <- 25 + newd$predA + 1.2*newd$predB + error
m2<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m2)

#13. Explain the difference between models m0 m1 and m2

# In m0, we have allowed fixed effects only. Whereas in both m1 and m2, we have used both
# fixed and random effect. The only change between m1 and m2 is we haven't considered 
# "newd$subjint", "newd$itemint" predictors to estimate the response values in m2 model.
# We also oberved that there is no major changes in m1 and m2. For verfication purpose, we may
# also use "resp" as the response variable in m0 model. 
m0<-lm(resp ~ predA + predB , data=lmerd)
summary(m0)
# We will observe a little change in m0 with "resp" than m0 with "respr" model.
# So, we may conclude that "newd$subjint", "newd$itemint" have very little affects on the 
# response variable.

#14. Play around with the size of the by item and by subject effects (here: intercepts only)
# we have taken the no of subjects  30 and item also  20
n<-600
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
error <- rnorm (n, 0, 30)
subjno <- 30 #number of subjects; 
itemno<- 20 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)
# basic data frame done; now on to by subject and by item random effects:
subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjeffdf<-data.frame(subjid, subjint)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here!
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m0<-lm(respr ~ predA + predB , data=lmerd)
summary(m0)
m1<-lmer(respr ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m1)

lmerd$resp <- 25 + newd$predA + 1.2*newd$predB + error
m2<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m2)


#15. Generate the data such that subjects differ in terms of how much predictor A affects them.
lmerd$resp <- 25 + newd$predA + error

#16. Then build a mixed effects model that includes a random slope for subjects.
mixed_model <-lmer(resp ~ predA + (1|subj), data=lmerd)
summary(mixed_model)

