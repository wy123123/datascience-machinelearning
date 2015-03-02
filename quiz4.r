
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
set.seed(33833)
head(vowel.train)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
modfit=train(y~.,method="rf",data=vowel.train)
confusionMatrix(vowel.test$y,predict(modfit,vowel.test))
modfit.2=train(y~.,method="gbm",data=vowel.train,verbose=F)
confusionMatrix(vowel.test$y,predict(modfit.2,vowel.test))



library(caret)
library(gbm)
set.seed(62433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
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


set.seed(233)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
modfit=train(CompressiveStrength~.,method="lasso",data=training)
varImp(modfit)


library(lubridate)  # For year() function below
dat = read.csv("C:/Users/Lovebonito/Downloads/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstesting=ts(testing$visitsTumblr)
require(forecast)
a=bats(tstrain)
predict(a,level=0.95)
s=forecast(a,level=0.95,h=235)
plot(tstrain)

require(e1071)
set.seed(325)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
model <- svm(CompressiveStrength ~ ., data = training)
predict(model,testing)
require(hydroGOF)
rmse(predict(model,testing),testing$CompressiveStrength)
