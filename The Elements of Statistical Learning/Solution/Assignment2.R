#4(a)
#install.packages('ISLR')
library(ISLR)
data(Auto)
#excluding name variable
neumeric_variables = Auto[sapply(Auto, is.numeric)]
plot(neumeric_variables)

#4(b)
#finding all correlation value with each other
cor_data = cor(neumeric_variables)

#4(c)
attach(Auto)

#linear regression with mpg as response using variable cylinder

lm.fit = lm(mpg ~ cylinders)
summary(lm.fit)
#R-squared value:  0.604689

#linear regression with mpg and displacement

lm.fit = lm(mpg ~ displacement)
summary(lm.fit)

#R-squared value:  0.6482294

#linear regression with mpg with horsepower

lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)

#R-squared value:  0.6059483

#linear regression with mpg with year

lm.fit = lm(mpg ~ year)
summary(lm.fit)

#R-squared value:  0.3370278


#4(d)
#linear regression with mpg as response with other variables as predictors

lm.fit = lm(mpg ~ .-name, data = Auto)
summary_all = summary(lm.fit)$r.sq
summary(lm.fit)
#multiple R-squared value: 0.8215

#4(e)

par(mfrow=c(2,2))
plot(lm.fit)


#4(f)

par(mfrow=c(3,4))
lm.fit1 = lm(weight  ~ (cylinders + displacement + horsepower  + acceleration + year + origin)^2)
lm.fit2 = lm(cylinders ~ ( displacement + horsepower + weight + acceleration + year + origin)^2  )
lm.fit3 = lm(year ~ (cylinders + displacement + horsepower + weight + acceleration  + origin)^2 )
summary(lm.fit1)
summary(lm.fit2)
summary(lm.fit3)
plot(lm.fit1, main = "pair wise with weight")
plot(lm.fit2, main = "pair wise with cylinder")
plot(lm.fit3, main = "pair wise with year")

# The R squared values for these cases are 0.9413, 0.9327 and 0.3136 respectively,
#So year-weight as predictors describe the models best among those three test cases.


lm.fit1 = lm(mpg ~ I(log(displacement)))
lm.fit2 = lm(mpg ~ I(sqrt(displacement)))
lm.fit3 = lm(mpg ~ I(displacement*displacement))
summary(lm.fit1)
summary(lm.fit2)
summary(lm.fit3)
plot(lm.fit1, main = "log")
plot(lm.fit2, main = "square root")
plot(lm.fit3, main = "square")

# The R squared values for these cases are 0.6863, 0.6746 and 0.566 respectively for log, square root and square of displacement.

