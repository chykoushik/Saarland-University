###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in dataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercise below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on running t tests for this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 

## done


# 2. Read in the data into a variable called "dat".

dat <- read.csv("digsym.csv")
# 3. Load the libraries languageR, stringr, dplyr and tidyr.

library("languageR")
library("stringr")
library("dplyr")
library("tidyr")

# 4. How many rows, how many columns does that data have?

nrow(dat)
ncol(dat)

# 5. Take a look at the structure of the data frame using "glimpse"

glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows

head(dat, 20)
tail(dat, 20)

# 7. Is there any missing data in any of the columns?


any(is.na(dat)) 
#As the function above returned TRUE, there is missing data in the columns.

# 8. Get rid of the row number column


dat <- dat[,-1]



# 9. Put the Sub_Age column second

dat1 <- dat[c("ExperimentName","Sub_Age","Group","Gender","List","SubTrial","StimulDS1.CRESP","StimulDS1.RESP", "StimulDS1.RT","File")]

# 10. Replace the values of the "ExperimentName" column with something shorter, more legible


dat1$ExperimentName <- "DSK" 
dat1


# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.



data2<-subset(dat1,List=="Trial:2")



# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate"

data2<-separate(data2, col = Sub_Age, into=c("Subject", "Age"))


# 13. Make subject a factor

data2$Subject<-factor(data2$Subject)

is.factor(data2$Subject) # For checking the result only


# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".


str_remove_all(data2$File, "[0-9_]")



# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 on the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc)

str_pad(data2$File, width = 8, side = "right", pad = 0)



# 16. Remove the column "List"

data2 <- data2[, -6]


# 17. Change the data type of "Age" to integer


data2$Age <- as.integer(data2$Age)


# 18. Missing values, outliers:
# do we have any NAs in the data, and if so, how many and where are they?

any(is.na(data2)) #To verify

#At this stage, we don't have any NAs in the dataset.

# 19. Create an "accuracy" column using if-statement
# if actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0

data2$accuracy <- ifelse(data2$StimulDS1.RESP==data2$StimulDS1.CRESP, 1, 0)

data2 #For checking the result only



# 20. How many wrong answers do we have in total?

# Plese note: 
#To make my calculation easier, we will clean the data of the column named File and keep values "right" and "wrong" only.

cleanFileValue <- str_remove_all(data2$File, "[0-9_]")

#Now we will count the wrongs in the "File" column

countWrong <- sum(cleanFileValue == "wrong")

countWrong #For checking the result only

# 21. Whats the percentage of wrong responses?

#I will create a function that will calculate the percentage

percentage <- function(x, y){
  return((x/y)*100)
}

# we will count the number of total values in File column:

lngFile <- length(data2$File)

lngFile #For checking the result only

#Now we will call my function to find out the percentage of "wrong" responses:

percentage(countWrong, lngFile)

# 22. Create a subset "correctResponses" that only contains those data points where subjects responded correctly. 

correctResponses<-subset(data2,File=="right")
correctResponses


# 23. Create boxplot of StimulDS1.RT - any outliers?
boxplot(data2$StimulDS1.RT)


#yes there are outliers

# 24. Create histogram of StimulDS1.RT with bins set to 50

hist(data2$StimulDS1.RT, breaks = 50)

# 25. Describe the two plots - any tails? any suspiciously large values?

tail(data2$StimulDS1.RT)
#yes, there is suspiciously large value, high value for boxplot and histogram is 14000.


# 26. View summary of correct_RT
summary(data2$StimulDS1.RT) 

# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named "cleaned".


remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
cleaned <- remove_outliers(data2$StimulDS1.RT)
boxplot(cleaned)

