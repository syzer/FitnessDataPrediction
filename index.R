library(gdata)
library(ggplot2)
library(ISLR)
#library(sqldf)
library(caret)

data  <- read.csv("./data/pml-training.csv")
dataDA <- read.csv("./data/pml-testing.csv")

#pack/discard data
nzv <- nearZeroVar(data)
nzvDA <- nearZeroVar(dataDA)

data <- data[, -nzv]
dataDA <- dataDA[, -nzvDA]


data <- subset(data, select=-c(X,max_roll_belt, var_accel_arm,
                               min_pitch_dumbbell,
                               amplitude_roll_dumbbell, amplitude_pitch_dumbbell,
                               var_accel_dumbbell,avg_roll_dumbbell,stddev_roll_dumbbell,var_roll_dumbbell,
                               avg_pitch_dumbbell,max_picth_forearm))

data <- subset(data, select=-c(max_picth_belt, min_roll_belt, min_pitch_belt, amplitude_roll_belt, amplitude_pitch_belt, var_total_accel_belt, avg_roll_belt, stddev_roll_belt, var_roll_belt, avg_pitch_belt, stddev_pitch_belt, var_pitch_belt, avg_yaw_belt, stddev_yaw_belt, var_yaw_belt, max_picth_arm, max_yaw_arm, min_yaw_arm, amplitude_yaw_arm, max_roll_dumbbell, max_picth_dumbbell, min_roll_dumbbell, stddev_pitch_dumbbell, var_pitch_dumbbell, avg_yaw_dumbbell, stddev_yaw_dumbbell, var_yaw_dumbbell, min_pitch_forearm, amplitude_pitch_forearm, var_accel_forearm))


#make test set
inTrain <- createDataPartition(y=data$classe, p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

#modFit <- train(training$classe ~ .,method="rpart", data=training)
modFit <- train(classe~., method="rpart", data=training)
#modFit <- train(classe~., method="nb", data=training)
#modFit <- train(classe~., method="gbm",data=testing,verbose=FALSE)
modFit<- train(classe~., data=training, method="rf", prox=TRUE)
#modFit<- train(classe~., data=testing, method="rf", prox=TRUE)

print(modFit$finalModel)    #randomForest
#on all variables
#No. of variables tried at each split: 61

#OOB estimate of  error rate: 16.72%
#Confusion matrix:
#  A  B  C  D  E class.error
#A 73  3  2  2  0   0.0875000
#B 12 40  2  2  1   0.2982456
#C  3  3 40  0  0   0.1304348
#D  3  1  4 39  1   0.1875000
#E  1  4  1  3 47   0.1607143
modFit
#Random Forest 
#13737 samples
#98 predictor
#5 classes: 'A', 'B', 'C', 'D', 'E' 
#No pre-processing
#Resampling: Bootstrapped (25 reps) 

#Resampling results across tuning parameters:
#mtry  Accuracy   Kappa      Accuracy SD  Kappa SD  
#2   0.7741288  0.7115778  0.04714555   0.05853568
#61   0.7756104  0.7146539  0.04196818   0.05298081
#120   0.7582975  0.6929876  0.04110298   0.05149169


# Na->0
training[is.na(training <- training)] <- 0
testing[is.na(testing <- testing)] <- 0
dataDA[is.na(dataDA <- dataDA)] <- 0

pred <- predict(modFit,testing) 
predRight <- pred==testing$classe
table(pred, testing$classe)    # like confusion matrix
#plot wrongly predicted
#qplot(classe,var_accel_dumbbel,colour=predRight,data=testing,main="newdata Predictions")
#pred   A    B    C    D    E
#A   1286  296   38   60   18
#B      0  161    1    0    0
#C    159  190  864  290  241
#D    226  492  123  614  348
#E      3    0    0    0  475



#DA - data to analysys
pred_DA <- predict(modFit,dataDA) 
pred_DA