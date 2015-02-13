#Q2 
#Make a plot of the outcome (CompressiveStrength) versus the index of the samples.
#Color by each of the variables in the data set (you may find the cut2() function in the Hmisc package useful 
#for turning continuous covariates into factors).
#What do you notice in these plots?
#------------------------------------#
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(ggplot2)
set.seed(975)
inTrain=createDataPartition(mixtures$CompressiveStrength,p=3/4)[[1]]
training=mixtures[ inTrain,]
testing=mixtures[-inTrain,]
library(Hmisc)
cut2(training$CompressiveStrength)
ggplot()+geom_point(aes(x=seq_along(CompressiveStrength),y=CompressiveStrength,colour=cut2(Water)),data=training)
#-------------------------------------#
#Q3
#Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log transform
#to try to make the data more symmetric. Why would that be a poor choice for this variable?
#-------------------------------------#
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain=createDataPartition(mixtures$CompressiveStrength,p=3/4)[[1]]
training=mixtures[inTrain,]
testing=mixtures[-inTrain,]
hist(log(training$Superplasticizer))
#-------------------------------------#
#Q4
#Find all the predictor variables in the training set that begin with IL. Perform principal components
#on these variables with the preProcess() function from the caret package.
#Calculate the number of principal components needed to capture 90% of the variance. How many are there?
#-------------------------------------#
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData=data.frame(diagnosis,predictors)
inTrain=createDataPartition(adData$diagnosis,p=3/4)[[1]]
training=adData[inTrain,]
testing=adData[-inTrain,]
dt=training[,grepl("IL",colnames(training))]
dt=subset(dt,select=-13)
pre=preProcess(dt,thresh=0.9,method="pca")
pre
#-------------------------------------#
#Q5
#Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. 
#Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining
#80% of the variance in the predictors. 
#Use method="glm" in the train function. What is the accuracy of each method in the test set? Which is more accurate?
#-------------------------------------#
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData=data.frame(diagnosis,predictors)
inTrain=createDataPartition(adData$diagnosis,p=3/4)[[1]]
training=adData[inTrain,]
testing=adData[-inTrain,]
dt=training[,grepl("IL",colnames(training))]
dt=subset(dt,select=-13)
pre=preProcess(dt,thresh=0.8,method="pca")   # find PCA that containing 80% of the variance in the predictors
trainP=predict(pre,dt)   #transform training data
df=train(training$diagnosis~.,method="glm",data=trainP)   #fit a glm model

dt.t=testing[,grepl("IL",colnames(testing))] #transform testing data
dt.t=subset(dt.t,select=-13)
testP=predict(pre,dt.t)

confusionMatrix(testing$diagnosis,predict(df,testP)) #confusion matrix of using PCA

df=train(training$diagnosis~.,method="glm",data=dt)
confusionMatrix(testing$diagnosis,predict(df,testing)) #confusion matrix not using PCA
