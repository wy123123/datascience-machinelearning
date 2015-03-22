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
dt.train.1 <- subset(dt.train,select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window,classe))
dt.test.1 <-subset(dt.test,select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window,problem_id)) 
#remove NA columns
dt.train.2=dt.train.1[,colSums(is.na(dt.train.1))<19622*0.1]
dt.test.2=dt.test.1[,colSums(is.na(dt.train.1))<19622*0.1]
#dt.test.2=dt.test.1[,colSums(is.na(dt.test.1))<20*0.1]
#near near 0 variance columns
dt.train.3 <- nearZeroVar(dt.train.2,saveMetrics=T)
dt.train.4 <- dt.train.2[,!dt.train.3$nzv]
#dt.test.3 <- nearZeroVar(dt.test.2,saveMetrics=T)
dt.test.4 <- dt.test.2[,!dt.train.3$nzv]
#PCA for further dimention reduction
pre <- preProcess(dt.train.4.v,method="pca")
#tansforme data
trainP=predict(pre,dt.train.4.v)
testP=predict(pre,dt.test.4)


#model selection
mod.1 <- train(dt.train.4$classe~.,method="rf",data=trainP)
mod.3 <- train(dt.train.4$classe~.,method="rpart",data=trainP)
mod.4 <- train(dt.train.4$classe~.,method="lda",data=trainP)

rfc=confusionMatrix(dt.train$classe,predict(mod.4,trainP))
rfc
varImp(mod.1)

#random forest is the most accurate model.

##################
#Cross validation, k-fold 
##################
set.seed(123)
#create 10 folds, find the average sample error of the 10 sets of data
folds.train <- createFolds(y=dt.train$classe,k=3,list=T,returnTrain=T)
folds.test<- createFolds(y=dt.train$classe,k=3,list=T,returnTrain=F)
cmatrix=list()
for(i in 1 : 3){
        
        dt.train.1 <- subset(dt.train[folds.train[[i]],],select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window))
        dt.test.1 <-subset(dt.train[folds.test[[i]],],select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window)) 
        #remove NA columns
        dt.train.2=dt.train.1[,colSums(is.na(dt.train.1))<length(folds.train[[i]])*0.1]
        dt.test.2=dt.test.1[,colSums(is.na(dt.train.1))<length(folds.train[[i]])*0.1]
        #dt.test.2=dt.test.1[,colSums(is.na(dt.test.1))<length(folds.test[[i]])*0.1]
        #near near 0 variance columns
        dt.train.3 <- nearZeroVar(dt.train.2,saveMetrics=T)
        dt.train.4 <- dt.train.2[,!dt.train.3$nzv]
        dt.test.3 <- nearZeroVar(dt.test.2,saveMetrics=T)
        dt.test.4 <- dt.test.2[,,!dt.test.3$nzv]
        #remove the response variable
        dt.train.4.v <- subset(dt.train.4,select=-classe)
        dt.test.4.v <- subset(dt.test.4,select=-classe)
        #PCA for further dimention reduction
        pre <- preProcess(dt.train.4.v,method="pca")
        #tansforme data
        trainP <- predict(pre,dt.train.4.v)
        testP <- predict(pre,dt.test.4.v)
        
        mod <- train(dt.train.4$classe~.,method="rf",data=trainP)
        cmatrix[[i]] <- confusionMatrix(dt.test.4$classe,predict(mod,testP))$overall
}

mean(as.numeric(unlist(lapply(cmatrix,'[[',1))))

answers = predict(mod.1,testP)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
