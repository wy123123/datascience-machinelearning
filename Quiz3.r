library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle,rpart.plot)
library(rpart)
inTrain=createDataPartition(y=segmentationOriginal$Case,p=0.80,list=FALSE)
training=segmentationOriginal[inTrain,]
testing=segmentationOriginal[-inTrain,]
set.seed(125)
modfit=train(Class~.,method="rpart",data=training)
fancyRpartPlot(modfit$finalModel)


library(pgmm)
data(olive)
olive = olive[,-1]
head(olive)
inTrain=createDataPartition(y=olive$Area,p=0.80,list=FALSE)
training=olive[inTrain,]
testing=olive[-inTrain,]
modfit=train(Area~.,method="rpart",data=training)
newdata = as.data.frame(t(colMeans(olive)))
predict(modfit,newdata=newdata)


library(ElemStatLearn)
data(SAheart)
set.seed(13234)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
head(trainSA)
modfit=train(chd~age+alcohol+obesity+tobacco+typea+ldl ,method="glm",family="binomial",data=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
#find miss rate of the training set
prediction=predict(modfit,trainSA)
missClass(trainSA$chd,prediction)
#find miss rate of the test set
prediction=predict(modfit,testSA)
missClass(testSA$chd,prediction)


library(ElemStatLearn)
library(randomForest)
data(vowel.train)
data(vowel.test) 
set.seed(33833)
head(vowel.train)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
modfit=train(y~.,method="rf",data=vowel.train)
varImp(modfit)
