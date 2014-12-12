machinL
=======

machine learning

Coursera project

FInal project
Blaz Skrlj

Thursday, December 11, 2014

library(knitr)
## Warning: package 'knitr' was built under R version 3.1.2
library(ElemStatLearn)
## Warning: package 'ElemStatLearn' was built under R version 3.1.2
library(caret)
## Warning: package 'caret' was built under R version 3.1.2
## Loading required package: lattice
## Loading required package: ggplot2
## Warning: package 'ggplot2' was built under R version 3.1.2
library(rpart)
library(randomForest)
## Warning: package 'randomForest' was built under R version 3.1.2
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
set.seed(444)
opts_chunk$set(echo = TRUE, results = 'hold')
#read the file via Rstudio
traindata <- read.csv("C:/Users/bibaleze69/Downloads/pml-training.csv", na.strings=c("", "NA", "NULL"))
traindata <- traindata[,-1]
inTrain = createDataPartition(traindata$classe, p=0.60, list=FALSE)
training = traindata[inTrain,]
validating = traindata[-inTrain,]

sum((colSums(!is.na(training[,-ncol(training)])) < 0.60*nrow(training)))
## [1] 100
CleanD <- c((colSums(!is.na(training[,-ncol(training)])) >= 0.70*nrow(training)))
training   <-  training[,CleanD]
validating <- validating[,CleanD]

model <- randomForest(classe~.,data=training)
print(model)
## 
## Call:
##  randomForest(formula = classe ~ ., data = training) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.16%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3348    0    0    0    0 0.0000000000
## B    2 2277    0    0    0 0.0008775779
## C    0    2 2051    1    0 0.0014605648
## D    0    0    9 1919    2 0.0056994819
## E    0    0    0    3 2162 0.0013856813
confusionMatrix(predict(model,newdata=validating[,-ncol(validating)]),validating$classe)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2232    1    0    0    0
##          B    0 1517    5    0    0
##          C    0    0 1363    3    0
##          D    0    0    0 1281    0
##          E    0    0    0    2 1442
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9986          
##                  95% CI : (0.9975, 0.9993)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9982          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9993   0.9963   0.9961   1.0000
## Specificity            0.9998   0.9992   0.9995   1.0000   0.9997
## Pos Pred Value         0.9996   0.9967   0.9978   1.0000   0.9986
## Neg Pred Value         1.0000   0.9998   0.9992   0.9992   1.0000
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2845   0.1933   0.1737   0.1633   0.1838
## Detection Prevalence   0.2846   0.1940   0.1741   0.1633   0.1840
## Balanced Accuracy      0.9999   0.9993   0.9979   0.9981   0.9998
#read the file via Rstudio
testdata <- read.csv("C:/Users/bibaleze69/Downloads/pml-testing.csv", na.strings=c("", "NA", "NULL"))

testdata <- testdata[,-1] # Remove the first column that represents a ID Row
testdata <- testdata[ , CleanD] # Keep the same columns of testing dataset
testdata <- testdata[,-ncol(testdata)] # Remove the problem ID

# Coerce testing dataset to same class and strucuture of training dataset 
testing <- rbind(training[100, -59] , testdata) 
# Apply the ID Row to row.names and 100 for dummy row from testing dataset 
row.names(testing) <- c(100, 1:20)
predictions <- predict(model,newdata=testing[-1,])
print(predictions)
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
#submission
answers = c("B","A","B","A","A","E","D","B","A","A","B","C","B","A" ,"E","E","A","B","B","B")
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
