### Stats with R Exercise sheet 9

##########################
#Week 10: Linear Mixed Effects Models
##########################




###########################################################################################
###########################################################################################
install.packages("lme4")
install.packages("lattice")
library(lme4)
library(lattice)
library(Matrix)

# Read in the data file from gender.Rdata, sem.Rdata or relclause.Rdata.
# You can choose the one you'd like best based on the description of the items below, 
# and explore the analysis for that dataset. Afterwards, you can adapt your analysis 
# for the other datasets (that should be considerably less work.)

# The files contain data from an experiment where people were reading sentences, 
# and pressed the space bar to see the next word. The duration for which a word was 
# viewed before pressing the space bar again is the reading time of the word, and is 
# stored in the file as "WORD_TIME". The experiment had 24 items (given as "ITEM_ID") 
# and 24 subjects (given as "PARTICIPANT"). The order in which the different sentences 
# were presented in the experiment is given in the variable "itemOrder". 

# For each of the files, the sentences that were shown had a different property. 

# Sentences in the sem.Rdata experiment had a semantic violation, i.e. a word that 
# didn't fit in with the previous words in terms of its meaning. The experiment 
# contained two versions of each item, which were identical to one another except 
# for the one sentence containing a semantic violation, while the other one was 
# semantically correct. These conditions are named "SG" for "semantically good" 
# and "SB" for "semantically bad".

# Semantic materials (the experiment is in German, English translation given 
# for those who don't speak German')

# Christina schießt / raucht eine Zigarette nach der Arbeit. 
# "Christina is shooting / smoking a cigarette after work."

# The crticial word here is "Zigarette", as this would be very surprising in the 
# context of the verb "schießt", but not in the context of the verb "smoke". 
# Reading times are comparable because the critical word "Zigarette" is identical 
# in both conditions.

# Syntactic items:
# Simone hatte eine(n) schreckliche(n) Traum und keine Lust zum Weiterschlafen. 
# "Simone had a[masc/fem] horrible[masc/fem] dreammasc and didn't feel like sleeping 
# any longer."

# Here, there are again two conditions, one using correct grammatical gender on 
# "einen schrecklichen" vs. the other one using incorrect grammatical gender 
# "eine schreckliche". The critical word is "Traum" (it's either consisten or 
# inconsistent with the marking on the determiner and adjective)

# Relative clause items:
# Die Nachbarin, [die_sg nom/acc einige_pl nom/acc der Mieter auf Schadensersatz  
# verklagt hat_sg/ haben_pl]RC, traf sich gestern mit Angelika. 
# "The neighbor, [whom some of the tenants sued for damages / who sued some of  the
# tenants for damages]RC, met Angelika yesterday."

# When reading such a sentence, people will usually interpret the relative pronoun 
# die as the subject of the relative clause and the following noun phrase 
# "einige der Mieter" as the object. This interpretation is compatible with 
# the embedded singular-marked (sg) verb hat at the end of the relative clause. 
# Encountering the verb haben, which has plural marking (pl), leads to processing 
# difficulty: in order to make sense of the relative clause, readers need to 
# reinterpret the relative pronoun die as the object of the relative clause 
# and the following noun phrase "einige der Mieter" as its subject. 
# (Note that the sentences are all grammatical, as the relative pronoun and 
# following NPs are chosen such that they are ambiguous between nominative (nom)
# and accusative (acc) case marking.)

# The number of the word in a sentence is given in column "SEMWDINDEX". 
# 0 designates the word where the semantic violation happens (in the SB condition; 
# in the SG condition, it's the corresponding word). We call this word the 
# "critical word" or "critical region". -1 is the word before that, -2 is 
# two words before that word, and 2 is two words after that critical word. 
# "EXPWORD" shows the words. We expect longer reading times for the violation 
# at word 0 or the word after that (word 1) (if people press the button quickly 
# before thinking properly).

#######################################################################################
#######################################################################################

# a) Take a look at the data.
getwd()
data_gender<-read.table("gender.Rdata")
data_gender
str(data_gender)
summary(data_gender)
data_sem<-read.table("sem.Rdata")
data_sem
summary(data_sem)
data_clause<-read.table("relclause.Rdata")
data_clause
summary(data_clause)
# b) Plot it (use ggplot for this task and all the tasks below).
#    (You can provide any plots we have seen so far to interpret the data.
#    For example, you can study the difference between the subjects (participants) 
#    in terms of responce time or the difference between items (sentences) in 
#    terms of response time).

#    Below you also find the plot for the dataset 'sleepstudy' from the package 'lme4'.
#    The figure shows relationships between days without sleeping and reaction 
#    time for each participant (subject) separately.

summary(sleepstudy)
print(xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
             layout = c(9,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Days of sleep deprivation",
             ylab = "Average reaction time (ms)"))

library(ggplot2)

#    Your task is also to figure out how to adapt this plot for our data. What do you 
#    conclude regarding the reading sentences experiment?
summary(data_gender)
ggplot(data_gender, aes(x = RELWDINDEX, y = WORD_TIME, group = PARTICIPANT, , color =PARTICIPANT)) + geom_boxplot() 
# We can see a lot of variation between the participants , with some participant having higher value of word time and 
#other having relatively lower.#We can see from the plot that most of the participants lie between RELWDINDEX  -4 to 4. We can aslo interpret 
#there is one word which is having  word time more than 6000.

# c) Decide whether you want to exclude any data points (provide not only the code,
#    but also a detailed (!) explanation);
# we decide to eliminate points whose word time exceeds 2200. and these points are considered as outliers to our experiment.

df2 <- subset(data_gender, WORD_TIME < 2200)
df2
ggplot(df2, aes(x = RELWDINDEX, y = WORD_TIME, group = PARTICIPANT, , color =PARTICIPANT)) + geom_boxplot() 


# d) Try to make a plot where for each word, the average reading 
#    (collapsing across items and subjects) is shown; in this plot all violations 
#    are at point 0. Of course, you should not collapse the semantically good vs. 
#    bad condition.

head(data_sem)
ITEM_TYPE_SB <- subset(data_sem, ITEM_TYPE=="SB")
ITEM_TYPE_SG <- subset(data_sem, ITEM_TYPE=="SG")

str(data_sem)
ggplot(ITEM_TYPE_SB, aes(x=factor(EXPWORD), y=WORD_TIME, group = ITEM_ID))+stat_summary(fun.y="mean", geom="bar") +
geom_bar(stat="identity")+
  ggtitle("for ITEM_TYPE_SB") #for ITEM_TYPE_SB

ggplot(ITEM_TYPE_SG, aes(x=factor(EXPWORD), y=WORD_TIME, group = ITEM_ID))+stat_summary(fun.y="mean", geom="bar")+
geom_bar(stat="identity")+
  ggtitle("for ITEM_TYPE_SG") #for ITEM_TYPE_SG

# e) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions (give a detailed explanation 
#    for each model).

#    Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific interception and slope. Adapt this plot for our study 
#    and make conclusions.
model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
model
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Subject"]])

model1 = lmer(WORD_TIME ~ RELWDINDEX + (1|PARTICIPANT), data_gender)
model1
#random effects
#here sd tells about the  how much variability is there in dependent measure of random effect, participant which is 143.
# here residuals stand for variability that is not due to Participant but
# this reflects the fact that  word time can  also be  affected by other factors that are outside of the socpe.

#fixed Effects

# The coefficient RELWDINDEX is the slope for the effect of each word for a PARTICIPANT. It means for one unit
# increase of the RELWDINDEX for a PARTICIPANT, the WORD TIME  will be increased by 6.483.
model2 = lmer(WORD_TIME ~ RELWDINDEX + (1|PARTICIPANT) + (1|ITEM_ID), data_gender)
model2

#random effects
#here sd tells about the  how much variability is there in dependent measure of random effect, participant which is 142.93, item id 28.46.
# we can see that there is more variability in the group Participant than item id.
# here residuals stand for variability that is not due to Participant but
# this reflects the fact that  word time can  also be  affected by other factors that are outside of the socpe.

#fixed Effects

# The coefficient RELWDINDEX is the slope for the effect of each word for a PARTICIPANT. It means for one unit
# increase of the RELWDINDEX for a PARTICIPANT, the WORD TIME  will be increased by 7.119. Here we can see that slope value is more 
#than in previous case by taking effect of item id.


model3 = lmer(WORD_TIME ~ RELWDINDEX + (RELWDINDEX|PARTICIPANT), data_gender)
model3
#random effects
#here sd tells about the  how much variability is there in dependent measure of random effect, participant which is 141.39.
#here we can see the slope value as 10.63  which indicates for every one unit increase in participant value slope increases by 10.63 times.
# here residuals stand for variability that is not due to Participant but
# this reflects the fact that  word time can  also be  affected by other factors that are outside of the socpe.

#fixed Effects

# The coefficient RELWDINDEX is the slope for the effect of each word for a PARTICIPANT. It means for one unit
# increase of the RELWDINDEX, the WORD TIME  will be increased by  6.519  
print(dotplot(ranef(model3,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["PARTICIPANT"]])
coef(model3)
# Making conclusions:

#We used R and lme4 to perform a linear mixed effects analysis of the relationship of
#the WORD_TIME  on each RELWDINDEX for the participants. As fixed effect, we have entered "RELWDINDEX" in the model,
#In the plot we are able to see intercept and slope value for each participant. 
#Visual inspection of the plot shows the noticeable deviation from homoscedasticity. 




