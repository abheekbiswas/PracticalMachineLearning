Project Introduction
--------------------

### Background

Usage of fitness trackers viz. Jawbone Up, Nike FuelBand & Fitbit has now made it possible to collect a large amount of data pertaining to personal activity. Although people regularly quantify level of a particular activity done by them, they seldom quantify how they do it.

### Objective

In this project, our objective will be to use data from accelerometers on the belt, forearm, arm and dumbbell of 6 participants who have been asked to perform barbell lifts correctly and incorrectly in 5 different ways. Using this data, our aim is to predict the manner in this they did the exercise.

``` r
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(knitr)
```

Getting and Loading Data
------------------------

The training data for this project are available here: <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here: <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source: <http://groupware.les.inf.puc-rio.br/har>.

``` r
set.seed(1111)

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
```

Partitioning the training set into myTraining and myTesting

``` r
inTrain <- createDataPartition(training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]
myTesting <- training[-inTrain, ]
dim(myTraining); dim(myTesting)
```

    ## [1] 11776   160

    ## [1] 7846  160

Cleaning the data
-----------------

Removing the NearZeroVariance variables

``` r
nzv <- nearZeroVar(myTraining, saveMetrics=TRUE)
myTraining <- myTraining[,nzv$nzv==FALSE]

nzv<- nearZeroVar(myTesting,saveMetrics=TRUE)
myTesting <- myTesting[,nzv$nzv==FALSE]
```

Removing the first column of myTraining dataset and cleaning variables with more than 60% NA

``` r
myTraining <- myTraining[c(-1)]

# Removing NA 
trainingV3 <- myTraining
for(i in 1:length(myTraining)) {
    if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .7) {
        for(j in 1:length(trainingV3)) {
            if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) == 1)  {
                trainingV3 <- trainingV3[ , -j]
            }   
        } 
    }
}

# Set back to the original variable name
myTraining <- trainingV3
rm(trainingV3)
```

Transform the myTesting and testing data sets

``` r
clean1 <- colnames(myTraining)
clean2 <- colnames(myTraining[, -58])  # remove the classe column
myTesting <- myTesting[clean1]         # allow only variables in myTesting that are also in myTraining
testing <- testing[clean2]             # allow only variables in testing that are also in myTraining

dim(myTesting)
```

    ## [1] 7846   58

``` r
dim(testing)
```

    ## [1] 20 57

Coercing data into same type

``` r
for (i in 1:length(testing) ) {
    for(j in 1:length(myTraining)) {
        if( length( grep(names(myTraining[i]), names(testing)[j]) ) == 1)  {
            class(testing[j]) <- class(myTraining[i])
        }      
    }      
}

# To get the same class between testing and myTraining
testing <- rbind(myTraining[2, -58] , testing)
testing <- testing[-1,]
```

Prediction with Data Trees
--------------------------

``` r
set.seed(1111)
modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")
rpart.plot(modFitA1)
```

![](PML-CourseProject-V1_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
predictionsA1 <- predict(modFitA1, myTesting, type = "class")
cmtree <- confusionMatrix(predictionsA1, myTesting$classe)
cmtree
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2149  201    7    2    0
    ##          B   61 1116   81   13    0
    ##          C   22  190 1260  145   56
    ##          D    0   11   20 1064  181
    ##          E    0    0    0   62 1205
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.8659          
    ##                  95% CI : (0.8582, 0.8734)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.8302          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9628   0.7352   0.9211   0.8274   0.8356
    ## Specificity            0.9626   0.9755   0.9362   0.9677   0.9903
    ## Pos Pred Value         0.9110   0.8780   0.7531   0.8339   0.9511
    ## Neg Pred Value         0.9849   0.9389   0.9825   0.9662   0.9640
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2739   0.1422   0.1606   0.1356   0.1536
    ## Detection Prevalence   0.3007   0.1620   0.2132   0.1626   0.1615
    ## Balanced Accuracy      0.9627   0.8553   0.9286   0.8975   0.9130

``` r
plot(cmtree$table, col = cmtree$byClass, main = paste("Decision Tree Confusion Matrix: Accuracy =", round(cmtree$overall['Accuracy'], 4)))
```

![](PML-CourseProject-V1_files/figure-markdown_github/unnamed-chunk-11-1.png)

Prediction with Random Forest
-----------------------------

``` r
set.seed(1111)
modFitB1 <- randomForest(classe ~ ., data=myTraining)
predictionB1 <- predict(modFitB1, myTesting, type = "class")
cmrf <- confusionMatrix(predictionB1, myTesting$classe)
cmrf
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2232    2    0    0    0
    ##          B    0 1516    4    0    0
    ##          C    0    0 1362    3    0
    ##          D    0    0    2 1283    1
    ##          E    0    0    0    0 1441
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9985          
    ##                  95% CI : (0.9973, 0.9992)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9981          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   0.9987   0.9956   0.9977   0.9993
    ## Specificity            0.9996   0.9994   0.9995   0.9995   1.0000
    ## Pos Pred Value         0.9991   0.9974   0.9978   0.9977   1.0000
    ## Neg Pred Value         1.0000   0.9997   0.9991   0.9995   0.9998
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2845   0.1932   0.1736   0.1635   0.1837
    ## Detection Prevalence   0.2847   0.1937   0.1740   0.1639   0.1837
    ## Balanced Accuracy      0.9998   0.9990   0.9976   0.9986   0.9997

``` r
plot(modFitB1)
```

![](PML-CourseProject-V1_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
plot(cmrf$table, col = cmtree$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cmrf$overall['Accuracy'], 4)))
```

![](PML-CourseProject-V1_files/figure-markdown_github/unnamed-chunk-14-1.png)

Prediction with Generalized Boosted Regression
----------------------------------------------

``` r
set.seed(1111)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1)

gbmFit1 <- train(classe ~ ., data=myTraining, method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)
```

    ## Loading required package: gbm

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:caret':
    ## 
    ##     cluster

    ## Loading required package: splines

    ## Loading required package: parallel

    ## Loaded gbm 2.1.3

    ## Loading required package: plyr

``` r
gbmFinMod1 <- gbmFit1$finalModel

gbmPredTest <- predict(gbmFit1, newdata=myTesting)
gbmAccuracyTest <- confusionMatrix(gbmPredTest, myTesting$classe)
gbmAccuracyTest
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2232    3    0    0    0
    ##          B    0 1514    3    0    0
    ##          C    0    1 1358    9    0
    ##          D    0    0    7 1272    6
    ##          E    0    0    0    5 1436
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.9957         
    ##                  95% CI : (0.9939, 0.997)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.9945         
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   0.9974   0.9927   0.9891   0.9958
    ## Specificity            0.9995   0.9995   0.9985   0.9980   0.9992
    ## Pos Pred Value         0.9987   0.9980   0.9927   0.9899   0.9965
    ## Neg Pred Value         1.0000   0.9994   0.9985   0.9979   0.9991
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2845   0.1930   0.1731   0.1621   0.1830
    ## Detection Prevalence   0.2849   0.1933   0.1744   0.1638   0.1837
    ## Balanced Accuracy      0.9997   0.9984   0.9956   0.9936   0.9975

``` r
plot(gbmFit1, ylim=c(0.9, 1))
```

![](PML-CourseProject-V1_files/figure-markdown_github/unnamed-chunk-16-1.png)

Cross Validation and Out of Sample Error Estimation
---------------------------------------------------

Random Forests gave an accuracy in the myTesting dataset of 99.85%, which was more accurate than what we got from the other two methods: Decition Tree or Generalized Boosted Regression (GBM). The expected out of sample error is 100% - 99.85% = 0.15%.

Predicting Results on Test Data
-------------------------------

Thus the result of the way they performed the exercise is as given below

``` r
predictionB2 <- predict(modFitB1, testing, type = "class")
predictionB2
```

    ##  1  2  3 41  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E
