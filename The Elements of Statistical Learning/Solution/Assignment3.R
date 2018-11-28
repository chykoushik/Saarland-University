# 4(a)

dataset = read.csv("phoneme.csv", header = TRUE)

train_set = subset(dataset, grepl('train', dataset$speaker))
train_data = train_set[, !(names(train_set) %in% c("row.names", "g", "speaker"))]

test_set = subset(dataset, grepl('test', dataset$speaker))
test_data = test_set[, !(names(test_set) %in% c("row.names", "g", "speaker"))]

#4(b)

train_set.aa.ao = subset(train_set, g %in% c('aa', 'ao'))

test_set.aa.ao = subset(test_set, g %in% c('aa', 'ao'))

train_data.aa.ao = train_set.aa.ao[, !(names(train_set.aa.ao) %in% c("row.names", "g", "speaker"))]

test_data.aa.ao = test_set.aa.ao[, !(names(test_set.aa.ao) %in% c("row.names", "g", "speaker"))]

#fitting the logistic regression model with train data 

glm.fits=glm(train_set.aa.ao$g ~ ., data = train_data.aa.ao, family=binomial(link="logit"))

#predicting the model over test and train data

glm.predict.train = predict(glm.fits, train_data.aa.ao , type = "response")
glm.predict.test = predict(glm.fits, test_data.aa.ao , type = "response")

#glm.predict.test
#train error
glm.error.train = (nrow(train_data.aa.ao) - sum(glm.predict.train)) / nrow(train_set.aa.ao)
glm.error.train  #0.4061033

#test error
glm.error.test = (nrow(test_data.aa.ao) - sum(glm.predict.test)) / nrow(test_set.aa.ao)
glm.error.test  #0.3863457

#4(c)

train_data.aa.ao = droplevels(train_data.aa.ao)

test_set.aa.ao = droplevels(test_set.aa.ao)

#Fit an LDA model
lda.fit.aa.ao = lda(train_set.aa.ao$g ~ ., data = train_data.aa.ao)
#prediction using the LDA model
lda.predict.train.aa.ao = predict(lda.fit.aa.ao, train_data.aa.ao)
lda.predict.test.aa.ao = predict(lda.fit.aa.ao, test_data.aa.ao)

#Compute train error
lda.error.train.aa.ao = (nrow(train_data.aa.ao) - sum(lda.predict.train.aa.ao$class == train_set.aa.ao$g)) / nrow(train_data.aa.ao)
lda.error.train.aa.ao   #0.1064163
test_set.aa.ao$g
#Compute test error
lda.error.test.aa.ao = (nrow(test_data.aa.ao) - sum(lda.predict.test.aa.ao$class == test_set.aa.ao$g)) / nrow(test_data.aa.ao)
lda.error.test.aa.ao   #0.214123

# 4(d)

#confusion matrix of LDA - train data
table(lda.predict.train.aa.ao$class, train_set.aa.ao$g)

#confusion matrix of LDA - test data
table(lda.predict.test.aa.ao$class, test_set.aa.ao$g)

#confusion matrix of logestic regression train data
table(as.numeric(glm.predict.train>0.5), train_set.aa.ao$g)

#confusion matrix of logestic regression - test data
table(as.numeric(glm.predict.test>0.5), test_set.aa.ao$g)

#4(e)

#Fit an LDA model
lda.fit.full = lda(train_set$g ~ ., data = train_data)
#prediction using the LDA model
lda.predict.train.all = predict(lda.fit.full, train_data)
lda.predict.test.all = predict(lda.fit.full, test_data)

#Compute train error
lda.error.train.all = (nrow(train_data) - sum(lda.predict.train.all$class == train_set$g)) / nrow(train_data)
lda.error.train.all   #0.05598802

#Compute test error
lda.error.test.all = (nrow(test_data) - sum(lda.predict.test.all$class == test_set$g)) / nrow(test_data)
lda.error.test.all   #0.080041061

#Plotting first two canonical coordinates of LDA
plot(lda.fit.full, dimen = 2)



