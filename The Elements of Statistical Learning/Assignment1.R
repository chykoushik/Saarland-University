load("Ozone.RData")

#Answer of 4(b):
#80 observation for train
str(trainset) # returns the datatypes of the datasets with all the values
summary(trainset) #displays the summary of dataset
dim(trainset) # returns the dimension of the dataset; 
range(trainset)

#31 observation for test
str(testset)
summary(testset)
length(testset)
range(testset)

#111 observation
ls(ozone)
str(ozone)
summary(ozone)
dim(ozone)
length(ozone)
range(ozone)
colnames(ozone) #displays column name 
head(ozone)
#column names "ozone" ,"radiation", "temperature", "wind" 
#222 observation total

#Answer 4(c):

pairs(ozone)  #plotted using pair function; image 1.1 in the pdf

par(mfrow=c(3,2)) 


plot(ozone$ozone, ozone$wind, xlab = "ozone", ylab = "wind", main = "Ozone vs Wind")
plot(ozone$ozone, ozone$temperature, xlab = "ozone", ylab = "temperature", main = "Ozone vs Temperature")
plot(ozone$ozone, ozone$radiation, xlab = "ozone", ylab = "radiation", main = "Ozone vs Radiation")
plot(ozone$wind, ozone$temperature, xlab = "wind", ylab = "temperature", main = "Wind vs Temperature")
plot(ozone$wind, ozone$radiation, xlab = "wind", ylab = "radiation", main = "Wind vs Radiation")
plot(ozone$radiation, ozone$temperature, xlab = "radiation", ylab = "temperature", main = "Radiation vs Temperature")

cor(ozone$ozone, ozone$wind )
cor(ozone$ozone, ozone$temperature)
cor(ozone$ozone, ozone$radiation) 
cor(ozone$wind,ozone$temperature)
cor(ozone$radiation, ozone$wind)
cor(ozone$radiation, ozone$temperature)

#Answer 4(d):

range(trainset) #range 1-111
mean(trainset) #mean  54.95
var(trainset)

range(testset)  #range 7-109
mean(testset) #mean is 58.70968
var(testset) # variance 687.0129

range(ozone$ozone)  #Range 1-168
range(ozone$radiation)#range 7 334
range(ozone$temperature)#range 57 97
range(ozone$wind)#range 20.7 2.3

mean(ozone$ozone) #Mean is 42.0991
mean(ozone$radiation)# mean is 184.8018
mean(ozone$temperature)#Mean is 77.79279
mean(ozone$wind)#Mean is 9.938739

var(ozone$ozone)# variance 1107.29
var(ozone$radiation)# variance 8308.742
var(ozone$temperature)# variance 90.82031
var(ozone$wind) #variance 12.66803


#Answer 4(e): 

#MSE Function is implemented here

MSE <- function(predicted_data = NULL, true_data = NULL){
  n = length(true_data)
  temp = 0
  for (i in 1:n){
    temp = temp + (true_data[i]- predicted_data[i])^2
  }
  return(temp/n)
}

#4(f):

# training the model for train data
trainset_lm = lm(formula = ozone ~ radiation + temperature + wind, data = ozone[trainset, ])
print(trainset_lm)

# predicting the train set to test the model
testset_predict = predict.lm(trainset_lm, ozone[testset,])
length(testset_predict)
true_values = ozone[testset,1]

MSE(testset_predict, true_values)
cor(testset_predict, true_values)#Pearson coefficient
par(mfrow=c(1,1)) 

#Scattered plot 
plot(testset_predict, true_values, xlab = "prediction value", ylab = "True Values", main = "Prediction values vs True Values")


#Answer 4(g):

#install.packages("FNN", dependencies = TRUE)

library(FNN)

trainset_data <- ozone[trainset,]
testset_data <- ozone[testset,]

#Perform KNN regression

mse_train_knn <- vector("double", 30)
mse_test_knn <- vector("double",30)

for (i in 1:30){
  knn.data_train <- knn.reg(trainset_data[,2:4], test = testset_data[,2:4], y = trainset_data[,1], k=i)
  mse_train_knn[i] = MSE( knn.data_train$pred, ozone[testset,1]) 
}

for (i in 1:30){
  knn.data_test <- knn.reg(trainset_data[,2:4], test = NULL , y = trainset_data[,1], k=i)
  mse_test_knn[i] = MSE(knn.data_test$pred, ozone[trainset,1])
}

i <- c(1:30)
par(mfrow=c(1,2))
plot(i, mse_train_knn, type="l", col="blue", xlab = "Number of neighbors", ylab = "MSE value", main = "Traning Data" )
plot(i, mse_test_knn, type="l", col="red", xlab = "Number of neighbors", ylab = "MSE value", main = "Test Data" )

