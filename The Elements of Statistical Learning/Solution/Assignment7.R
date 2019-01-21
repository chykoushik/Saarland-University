#assignment 7
#-------4(a)-----------

#load the dataset
data = load('wdbc.RData')

set.seed(123)

#create first 400 as training, and the remaining as test observations.
train_i = 1:400
train = wdbc[train_i,]
test = wdbc[-train_i,]

#-------4(b)-----------

library (randomForest)

rf.fit <- randomForest(Subtype ~., importance =TRUE, data = train)

#predicting on train set
pred.train <- predict(rf.fit, newdata = train)

# Checking classification accuracy for train set
table(pred.train, train$Subtype) 

#train set misclassification error
mean(pred.train != train$Subtype)

#predicting on test set
pred.test <- predict(rf.fit, newdata = test)

# Checking classification accuracy for test set
table(pred.test, test$Subtype) 

#test set misclassification error
mean(pred.test != test$Subtype)

importance(rf.fit)
#-------4(c)-----------

library (e1071)
library(caret)

set.seed(3233)
#SVM Radial on train
svm_Radial_model <- svm(Subtype ~., data=train, kernel ="radial",scale =TRUE)

#SVM Radial on train predict
svm_Radial_train_pred <- predict(svm_Radial_model, train)

# Checking classification accuracy for train set
table(svm_Radial_train_pred, train$Subtype) 

#SVM radial error on train
mean(svm_Radial_train_pred!=train$Subtype)


#SVM Radial on test predict
svm_Radial_test_pred <- predict(svm_Radial_model, test)

# Checking classification accuracy for test set
table(svm_Radial_test_pred, test$Subtype) 

#SVM radial error on test
mean(svm_Radial_test_pred!=test$Subtype)


#SVM polnomial on train
svm_polynomial_model <- svm(Subtype ~., data=train, kernel ="polynomial",scale =TRUE)

#SVM polynomial on train predict
svm_polynomial_train_pred <- predict(svm_polynomial_model, train)

# Checking classification accuracy for train set
table(svm_polynomial_train_pred, train$Subtype) 

#SVM polynomial error on train
mean(svm_polynomial_train_pred!=train$Subtype)

#SVM polynomial on test predict
svm_polynomial_test_pred <- predict(svm_polynomial_model, test)

# Checking classification accuracy for test set
table(svm_polynomial_test_pred, test$Subtype) 

#SVM polynomial error on test
mean(svm_polynomial_test_pred!=test$Subtype)


