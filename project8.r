############################################################################################
## Coursera Practical Machine Learning Course Project                                     ##
##                                                                                        ##
## PROGRAM: C:\Users\swenck\Documents\R\Coursera\Data Science\Course 8\Project\project8.r ##
## AUTHOR:  Steve Wenck                                                                   ##
## DATE:    October 22, 2017                                                              ##
############################################################################################

## Set working directory
setwd("~/R/Coursera/Data Science/Course 8/Project")

## Load needed packages
library(caret)
library(randomForest)

## Download the training dataset
destfile="pml-training.csv"
fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
if (!file.exists(destfile)) {
    download.file(fileURL,destfile,method="auto")
}

## Download the testing dataset
destfile="pml-testing.csv"
fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if (!file.exists(destfile)) {
    download.file(fileURL,destfile,method="auto")
}

## Read the training dataset
trainingMaster <- read.csv("pml-training.csv", na.strings = c("NA", ""))

## Read the testing dataset (to be used as validation)
validationMaster <- read.csv("pml-testing.csv", na.strings = c("NA", ""))

## Remove ID variables from the training dataset
trainingMaster <- subset(trainingMaster, select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window,num_window))

## Detect and remove Near Zero Variance Predictors
nzv <- nearZeroVar(trainingMaster, saveMetrics=TRUE)
trainingMaster <- trainingMaster[,nzv$nzv==FALSE]

## Remove variables with more than 70% NAs
trainingNAs <- trainingMaster
for(i in 1:length(trainingMaster)) {
    if( sum( is.na( trainingMaster[, i] ) ) /nrow(trainingMaster) > 0.7) {
        for(j in 1:length(trainingNAs)) {
            if( length( grep(names(trainingMaster[i]), names(trainingNAs)[j]) ) == 1)  {
                trainingNAs <- trainingNAs[ , -j]
            }
        }
    }
}
trainingMaster <- trainingNAs

## Set the seed for reproducibility
set.seed(2416)

## Split Training dataset into a training and testing dataset (leaving original testing dataset for validation)
trainPart <- createDataPartition(trainingMaster$classe, p=0.7, list=FALSE)
training <- trainingMaster[trainPart, ]
testing <- trainingMaster[-trainPart, ]

## Train using Random Forest
##model <- train(classe ~ .,  preProcess=c("center", "scale"), data = training, method="rf")
##model <- randomForest(training[,1:ncol(training)-1], training[,ncol(training)])
##controlRf <- trainControl(method="cv", 5)
##model <- train(classe ~ ., data=training, preprocess=c("center","scale"), method="rf", trControl=trainControl(method="cv", 5), ntree=250)
model <- train(classe ~ ., data=training, method="rf", trControl=trainControl(method="cv", 5), ntree=250)
model

## Estimate performance on test data
predictRf <- predict(model, testing)
confusionMatrix(testing$classe, predictRf)

## Calculate prediction accurancy
accuracy <- postResample(predictRf, testing$classe)
accuracy

## Calculate out-of-sample error
oose <- 1 - as.numeric(confusionMatrix(testing$classe, predictRf)$overall[1])
oose
