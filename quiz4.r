#Q1
#Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
#Set the variable y to be a factor variable in both the training and test set. 
#Then set the seed to 33833. Fit (1) a random forest predictor relating the factor variable y 
#to the remaining variables and (2) a boosted predictor using the "gbm" method. 
#Fit these both with the train() command in the caret package. 
#What are the accuracies for the two approaches on the test data set? 
#What is the accuracy among the test set samples where the two methods agree?
set.seed(33833)
head(vowel.train)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
modfit=train(y~.,method="rf",data=vowel.train)
confusionMatrix(vowel.test$y,predict(modfit,vowel.test))
modfit.2=train(y~.,method="gbm",data=vowel.train,verbose=F)
confusionMatrix(vowel.test$y,predict(modfit.2,vowel.test))

#Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"),
#boosted trees ("gbm") and linear discriminant analysis ("lda") model.
#Stack the predictions together using random forests ("rf").
#What is the resulting accuracy on the test set? Is it better or worse than each of the individual predictions?
set.seed(62433)
modfit.1=train(diagnosis~.,method="rf",data=training)
modfit.2=train(diagnosis~.,method="gbm",data=training,verbose=F)
modfit.3=train(diagnosis~.,method="lda",data=training)
confusionMatrix(testing$diagnosis,predict(modfit.1,testing))
confusionMatrix(testing$diagnosis,predict(modfit.2,testing))
confusionMatrix(testing$diagnosis,predict(modfit.3,testing))
pred.1=predict(modfit.1,testing)
pred.2=predict(modfit.2,testing)
pred.3=predict(modfit.3,testing)
comb=data.frame(pred.1,pred.2,pred.3,diagnosis=testing$diagnosis)
combfit=train(diagnosis~.,method="rf",data=comb)
confusionMatrix(testing$diagnosis,predict(combfit,comb))

#Q3
#set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
#Set the seed to 233 and fit a lasso model to predict Compressive Strength.
#Which variable is the last coefficient to be set to zero as the penalty increases?
#(Hint: it may be useful to look up ?plot.enet).
set.seed(233)
modfit=train(CompressiveStrength~.,method="lasso",data=training)
plot.enet(modfit$finalModel, xvar = "penalty", use.color = TRUE)


#Q4
#Load the data on the number of visitors to the instructors blog from here: 
#https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv
library(lubridate)  # For year() function below
dat = read.csv("~/Desktop/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
#Fit a model using the bats() function in the forecast package to the training time series.
#Then forecast this model for the remaining time points.
#For how many of the testing points is the true value within the 95% prediction interval bounds?
require(forecast)
model=bats(tstrain)
pred=forecast(model,h=235)
s=(testing$visitsTumblr<=pred$upper[,2]) * (testing$visitsTumblr>=pred$lower[,2])
table(s)
226/235*100

#Q5
#Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
#Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive 
#Strength using the default settings. 
#Predict on the testing set. What is the RMSE?
require(e1071)
set.seed(325)
model <- svm(CompressiveStrength ~ ., data = training)
require(hydroGOF)
rmse(predict(model,testing),testing$CompressiveStrength)
