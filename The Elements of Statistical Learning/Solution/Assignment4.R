# 4(a)
#read data from txt file
dataset = read.table('prostate.txt')

numeric_data = dataset[sapply(dataset, is.numeric)]

#Normalize columns
normalized_data = scale(numeric_data)

apply(normalized_data, 2, sd)

normalized_data = as.data.frame(normalized_data)
normalized_data$train <- dataset$train

train_set <- subset(normalized_data, grepl("^TRUE", normalized_data$train))
test_set <- subset(normalized_data, grepl("^FALSE", normalized_data$train))

train_data = train_set[sapply(train_set, is.numeric)]
test_data = test_set[sapply(test_set, is.numeric)]


#4 (b)
#install.packages("leaps")
library (boot)

set.seed (10)
#LOOCV 
glm.fit=glm(lpsa ~ lweight + age + lbph + svi + lcp + gleason + pgg45 ,data=train_data)
cv.err =cv.glm(train_data ,glm.fit)
cv.err$delta #0.6346332 0.6334927

# 5 - fold cross validation 
cv.err.5 = rep (0 ,5)
for (i in 1:5) {
  glm.fit=glm(lpsa~lweight + age + lbph + svi + lcp + gleason + pgg45 ,data=train_data)
  cv.err.5[i] = cv.glm (train_data ,glm.fit ,K=5) $delta [1]
}
cv.err.5
#0.6002520 0.7342732 0.6282413 0.6446386 0.7699189
# 10 - fold cross validation 
cv.err.10 = rep (0 ,10)
for (i in 1:10) {
  glm.fit=glm(lpsa~ lweight + age + lbph + svi + lcp + gleason + pgg45 ,data=train_data)
  cv.err.10[i] = cv.glm (train_data ,glm.fit ,K=10) $delta [1]
}
cv.err.10
#0.6388465 0.6725443 0.6159349 0.6397612 0.6617682 0.6174432 0.6171515 0.6849523 0.6120526 0.6498701
#fiting linear model
fit_lm <- lm(formula = lpsa~.,data = train_data)
lm_test_error = mean((test_data$lpsa -predict (fit_lm ,test_data))^2)
lm_coefficient = coefficients(fit_lm)
lm_test_error #0.3912072

# 4(c)

#install.packages("glmnet")
library(glmnet)

x = model.matrix(lpsa~., train_data)[,-1][,-9]
y = train_data$lpsa

#fit ridge regression models
ridge_model = glmnet (x,y,alpha =0)

par(mfrow = c(1,1))
plot(ridge_model, xvar="lambda",col=c(1:8))
legend("topright", inset = .05, title = "Coefficients",legend= names(train_data)
       ,fill = c(1:8),cex=.85 )


# 4(d)
# fit ridge regression models 
cv_out = cv.glmnet(x,y,nfolds = 10, alpha=0)
plot(cv_out)
#fiding the optimal value
best_lambda = cv_out$lambda.min
best_lambda  #0.08356111

x_test = model.matrix(lpsa~., test_data)[,-1][,-9]
y_test = test_data$lpsa

pred_train = predict(ridge_model, s=best_lambda, newx = x)
pred_test = predict(ridge_model, s=best_lambda, newx = x_test)

mse_train = mean((pred_train - y)^2)
mse_test = mean((pred_test - y_test)^2)

mse_train  #0.3366517
mse_test  #0.3701526

ridge_coefficients = predict(ridge_model, type="coefficients", s=best_lambda)
ridge_coefficients
#(Intercept) -0.009835612
#lcavol       0.496627023
#lweight      0.222528435
#age         -0.093351080
#lbph         0.172569283
#svi          0.241975578
#lcp         -0.133774317
#gleason      0.012480077
#pgg45        0.169316257

# 4(e)

lasso_model = glmnet(x, y, alpha=1)
plot(lasso_model, xvar="lambda",col = c(1:8))
legend("topright", inset = .001, title = "Coefficients",legend= names(train_data)
       ,bty="n",bg="transparent",fill = c(1:8) )

# 4(f)
#10-fold cross-validation
cv_out_lasso = cv.glmnet(x,y,nfolds = 10, alpha=1)
plot(cv_out_lasso)
best_lambda_lasso = cv_out_lasso$lambda.min
best_lambda_lasso #0.005009357


pred_train_lasso = predict(lasso_model, s=best_lambda_lasso, newx = x)
pred_test_lasso = predict(lasso_model, s=best_lambda_lasso, newx = x_test)

mse_train_lasso = mean((pred_train_lasso - y)^2)
mse_test_lasso = mean((pred_test_lasso - y_test)^2)

mse_train_lasso  #0.3302166
mse_test_lasso  #0.3796075
lasso_coefficients = predict(lasso_model, type="coefficients", s=best_lambda_lasso)
lasso_coefficients

# 4(g)
#Fitting linear model
fit_lm <- lm(formula = lpsa~.,data = train_data)
summary(fit_lm)
coefficients(fit_lm)
lm_coefficient = coefficients(fit_lm)
lm_train_pred = lm_coefficient[1] + lm_coefficient[2]*train_data$lcavol + lm_coefficient[3]*train_data$lweight + lm_coefficient[4]*train_data$age + lm_coefficient[5]*train_data$lbph + lm_coefficient[6]*train_data$svi + lm_coefficient[7]*train_data$lcp + lm_coefficient[8]*train_data$gleason + lm_coefficient[9]*train_data$pgg45
lm_train_error = mean((train_data$lpsa-lm_train_pred)^2)

lm_test_pred = lm_coefficient[1] + lm_coefficient[2]*test_data$lcavol + lm_coefficient[3]*test_data$lweight + lm_coefficient[4]*test_data$age + lm_coefficient[5]*test_data$lbph + lm_coefficient[6]*test_data$svi + lm_coefficient[7]*test_data$lcp + lm_coefficient[8]*test_data$gleason + lm_coefficient[9]*test_data$pgg45
lm_test_error = mean((test_data$lpsa-lm_test_pred)^2)

lm_train_error  #0.3296119
lm_test_error  #0.3912072

#Comparing coefficients
print('Ridge Coefficients')
ridge_coefficients

print('Lasso Coefficients')
lasso_coefficients

print('LM Coefficients')
lm_coefficient

