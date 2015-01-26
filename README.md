##HOWTO

## Load libraries

```r
library(gdata)
library(ggplot2)
library(foreach)
library(doParallel)
library(caret)
registerDoParallel()
set.seed(100)
options(warn=-1)
```

## Load and clean data and engineer features

```r
data  <- read.csv("./data/pml-training.csv", na.strings=c("", "#DIV/0!"))
dataDA <- read.csv("./data/pml-testing.csv",na.strings=c("", "#DIV/0!"))

features <- colnames(data[colSums(is.na(data)) == 0])[-(1:7)]
features2 <- colnames(dataDA[colSums(is.na(dataDA)) == 0])[-(1:7)]
data <- data[features]
dataDA <- dataDA[features2]

#convert to numeric if possible 
for(i in c(8:ncol(data)-1)) {data[,i] = as.numeric(as.character(data[,i]))}
for(i in c(8:ncol(dataDA)-1)) {dataDA[,i] = as.numeric(as.character(dataDA[,i]))}
```

## Make test set
used later for cross validation
```
inTrain <- createDataPartition(y=data$classe, p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]
dim(training)
dim(testing)
```

```
## [1] 13737    79
## [1] 5885   79
```

## Remove NA
```r
# Na->0, rf performs badly with NA
training[is.na(training <- training)] <- 0
testing[is.na(testing <- testing)] <- 0
dataDA[is.na(dataDA <- dataDA)] <- 0
```

## Build a model

Random forest is used

```r
#100 trees packed in 4 random forests  
x <- training[-ncol(training)]
y <- training$classe

rf <- foreach(ntree=rep(100, 4), .combine=randomForest::combine, .packages='randomForest') %dopar% {
  randomForest(x, y, ntree=ntree) 
}
```

## Evaluation on model
```r
predTraining <- predict(rf, newdata=training)
#confusionMatrix(predTraining,training$classe)
predTesting <- predict(rf, newdata=testing)
confusionMatrix(predTesting,testing$classe)
```

## Cross validation error estimation
```
##                Accuracy : 0.9949          
##                  95% CI : (0.9927, 0.9966)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9936  
```

## Finally predictions on test set
```r
pred_DA <- predict(rf, newdata=dataDA) #variables in the training data missing in newdata
pred_DA
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

