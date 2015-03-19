library(caret)
library(MASS)

url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
file <- file.path(getwd(),"m.csv")
download.file(url,file)
dt.train <- read.csv(file)

url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
file <- file.path(getwd(),"t.csv")
download.file(url,file)
dt.test <- read.csv(file)


##################
#feature selection
##################
dt.train.1 <- subset(dt.train,select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window))
dt.test.1 <-subset(dt.test,select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window)) 
#remove NA columns
dt.train.2=dt.1[,colSums(is.na(dt.1))<19622*0.1]
dt.test.2=dt.1[,colSums(is.na(dt.1))<20*0.1]
#near near 0 variance columns
dt.train.3 <- nearZeroVar(dt.train.2,saveMetrics=T)
dt.train.4 <- dt.train.2[,!dt.train.3$nzv]
dt.test.3 <- nearZeroVar(dt.test.2,saveMetrics=T)
dt.test.4 <- dt.test.2[,!dt.test.3$nzv]
#remove the response variable
dt.train.4.v <- subset(dt.train.4,select=-classe)
dt.test.4.v <- subset(dt.test.4,select=-classe)
#PCA for further dimention reduction
pre <- preProcess(dt.train.4.v,method="pca")
#tansforme data
trainP=predict(pre,dt.train.4.v)
testP=predict(pre,dt.test.4.v)

#model selection
mod.1 <- train(dt.train.4$classe~.,method="rf",data=trainP)
mod.2 <- train(dt.train.4$classe~.,method="gbm",data=trainP,verbose=F)
mod.3 <- train(dt.train.4$classe~.,method="rpart",data=trainP)
mod.4 <- train(dt.train.4$classe~.,method="lda",data=trainP)

confusionMatrix(dt.test.4$classe,predict(mod.3,testP))
varImp(mod.1)

#random forest is the most accurate model.

##################
#Cross validation, k-fold 
##################
set.seed(123)
#create 10 folds, find the average sample error of the 10 sets of data
folds.train <- createFolds(y=dt.train.4$classe,k=10,list=T,returnTrain=T)
folds.test<- createFolds(y=dt.train.4$classe,k=10,list=T,returnTrain=F)
cmatrix=0
for(i in 1 : 10){
        
        dt.train <- subset(dt.train[folds.train[[i]],],select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window))
        dt.test.1 <-subset(dt.train[folds.test[[i]],],select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window)) 
        #remove NA columns
        dt.train.2=dt.1[,colSums(is.na(dt.1))<length(folds.train[[i]])*0.1]
        dt.test.2=dt.1[,colSums(is.na(dt.1))<length(folds.test[[i]])*0.1]
        #near near 0 variance columns
        dt.train.3 <- nearZeroVar(dt.train.2,saveMetrics=T)
        dt.train.4 <- dt.train.2[,!dt.train.3$nzv]
        dt.test.3 <- nearZeroVar(dt.test.2,saveMetrics=T)
        dt.test.4 <- dt.test.2[,!dt.test.3$nzv]
        #remove the response variable
        dt.train.4.v <- subset(dt.train.4,select=-classe)
        dt.test.4.v <- subset(dt.test.4,select=-classe)
        #PCA for further dimention reduction
        pre <- preProcess(dt.train.4.v,method="pca")
        #tansforme data
        trainP=predict(pre,dt.train.4.v)
        testP=predict(pre,dt.test.4.v)
        
        mod <- train(dt.train.4$classe~.,method="rf",data=trainP)
        cmatrix[i]=confusionMatrix(dt.test.4$classe,predict(mod.3,testP))
}

avg.accurate=
