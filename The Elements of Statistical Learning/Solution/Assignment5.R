#assignment_5
dataset = load('prostate.Rdata')
#4(a)

library(leaps)
set.seed(3)

best_subset = regsubsets(prostate.train$lpsa~., prostate.train)
best_subset_summary = summary(best_subset)

par(mfrow = c(2,2))

#Plot Rsquare
plot (best_subset_summary$rsq ,xlab=" Number of features ",ylab=" RSQ",type="l")
which.min(best_subset_summary$rsq) #RSQ min at p =7

points(7, best_subset_summary$rsq[7], col ="red",cex =2, pch =20)

#Plot Adjusted RSquare
plot (best_subset_summary$adjr2 ,xlab=" Number of features ",ylab=" ADJR2",type="l")
which.max(best_subset_summary$adjr2) #Adj. rsq min max at p = 7

points(7, best_subset_summary$adjr2[7], col ="red",cex =2, pch =20)

#Plot CP
plot (best_subset_summary$cp ,xlab=" Number of features ",ylab=" CP",type="l")
which.min(best_subset_summary$cp) #cp min at p = 7

points(7, best_subset_summary$cp[7], col ="red",cex =2, pch =20)

#Plot BIC
plot (best_subset_summary$bic ,xlab=" Number of features ",ylab=" BIC",type="l")
which.min(best_subset_summary$bic) #bic min at p = 2

points(2, best_subset_summary$bic[2], col ="red",cex =2, pch =20)

plot(best_subset, scale="r2")
plot(best_subset, scale="adjr2")
plot(best_subset, scale="Cp")
plot(best_subset, scale="bic")

coefficient = coef(best_subset, 7)
coefficient
train_matrix = model.matrix(lpsa~.,prostate.train)
test_matrix = model.matrix(lpsa~.,prostate.test)

train_prediction = train_matrix [,names(coefficient)]%*% coefficient
train_error = mean(( prostate.train$lpsa-train_prediction)^2)

test_prediction = test_matrix [,names(coefficient)]%*% coefficient
test_error = mean(( prostate.test$lpsa-test_prediction)^2)

train_error  #0.4608833113
test_error  #0.47411941390
#4(b)
library(pls)
set.seed(2)
par(mfrow = c(1,2))
pcr.fit = pcr(prostate.train$lpsa~., data = prostate.train, scale=TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

mat = matrix(nrow = 8, ncol = 2)

for (i in 1:8){
  pcr.predict = predict(pcr.fit, prostate.test, ncomp = i)
  mat[[i,1]] = i
  mat[[i,2]] = mean((pcr.predict - prostate.test$lpsa)^2)
}


plot(mat, xlab = "No of M", ylab = "MSE", main = "Test Error", col = "red")
points(mat[[6,1]], mat[[6,2]], col = "blue", cex = 2)
summary(pcr.predict)
mat

#4(c)

library(pls)

pls.fit = plsr(prostate.train$lpsa~., data = prostate.train, scale = TRUE, validation = "CV")
summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")

mat = matrix(nrow = 8, ncol = 2)

for (i in 1:8){
  pls.predict = predict(pls.fit, prostate.test, ncomp = i)
  mat[[i,1]] = i
  mat[[i,2]] = mean((pls.predict - prostate.test$lpsa)^2)
}


plot(mat, xlab = "No of M", ylab = "MSE", main = "Test Error", col="red")
points(mat[[3,1]], mat[[3,2]], col = "blue", cex = 2) #min mse


#Using PCA
data = subset(prostate.train, select = -c(9))
pr.out = prcomp(data, scale = TRUE)
summary(pr.out)

names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation

std_dev = pr.out$sdev
pr_var = std_dev^2
pr_var
prop_varex = pr_var/sum(pr_var)
prop_varex

plot(prop_varex, xlab = "principal Component", ylab = "Propotion of variance explained", type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


#4(d)

total_dataset = rbind(prostate.train, prostate.test)

apply(total_dataset, 2, mean)

apply(total_dataset, 2, var)

par(mfrow = c(2,2))

plot(total_dataset$lpsa, total_dataset$lcavol, col = ifelse(total_dataset$lpsa > 2.5,"red", "blue"), main = "lpsa vs. lcavol")
plot(total_dataset$lpsa, total_dataset$lweight, col = ifelse(total_dataset$lpsa > 2.5,"red", "blue"), main = "lpsa vs. lweight")
plot(total_dataset$lpsa, total_dataset$age, col = ifelse(total_dataset$lpsa > 2.5,"red", "blue"), main = "lpsa vs. age")
plot(total_dataset$lpsa, total_dataset$lbph, col = ifelse(total_dataset$lpsa > 2.5,"red", "blue"), main = "lpsa vs. lbph")

plot(total_dataset$lpsa, total_dataset$svi, col = ifelse(total_dataset$lpsa > 2.5,"red", "blue"), main = "lpsa vs. svi")
plot(total_dataset$lpsa, total_dataset$lcp, col = ifelse(total_dataset$lpsa > 2.5,"red", "blue"), main = "lpsa vs. lcp")
plot(total_dataset$lpsa, total_dataset$gleason, col = ifelse(total_dataset$lpsa > 2.5,"red", "blue"), main = "lpsa vs. gleason")
plot(total_dataset$lpsa, total_dataset$pgg45, col = ifelse(total_dataset$lpsa > 2.5,"red", "blue"), main = "lpsa vs. pgg45")

#4(e)

# ploting on training dataset based on first 4 Principal component
plot(prostate.train$lpsa, prostate.train$lweight, col = ifelse(prostate.train$lpsa > 2.5,"red", "blue"), main = "lpsa_train vs. lweight_train")
plot(prostate.train$lpsa, prostate.train$lbph, col = ifelse(prostate.train$lpsa > 2.5,"red", "blue"), main = "lpsa_train vs. lbph_train")
plot(prostate.train$lpsa, prostate.train$gleason, col = ifelse(prostate.train$lpsa > 2.5,"red", "blue"), main = "lpsa_train vs. gleason_train")
plot(prostate.train$lpsa, prostate.train$pgg45, col = ifelse(prostate.train$lpsa > 2.5,"red", "blue"), main = "lpsa_train vs. pgg45_train")

