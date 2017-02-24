---
title: "run_analysis"
author: "B. Li"
date: "February 12, 2017"
output: html_document
---



# Final Project

## Preparation

Load the libraries.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(reshape2)
```

## Load the data

Assume the data package has already been downloaded and unzipped in the working directory. Here we load the data.


```r
path <- file.path(getwd(), "UCI HAR Dataset")
subjectTrain <- read.table(file.path(path, "train", "subject_train.txt"))
subjectTest  <- read.table(file.path(path, "test" , "subject_test.txt" ))
activityTrain <- read.table(file.path(path, "train", "y_train.txt"))
activityTest <- read.table(file.path(path, "test", "y_test.txt"))
dataTrain <- read.table(file.path(path, "train", "x_train.txt"))
dataTest <- read.table(file.path(path, "test", "x_test.txt"))
features <- read.table(file.path(path, "features.txt"))
activityLabels <- read.table(file.path(path, "activity_labels.txt"))
```

## Extract only the mean() and std() data

Get the column indices of the mean() and std() data, and extract these data from the training and the test sets.


```r
colnames(features) <- c("num", "name")
index <- features$num[grep("mean\\(\\)|std\\(\\)", features$name)]
dataTrain <- dataTrain[, index]
dataTest <- dataTest[, index]
```

## Merge the training and test data

Merge the training and test sets, and add the subject and activity values.


```r
colnames(subjectTrain) <- "Subject"
colnames(subjectTest) <- "Subject"
colnames(activityTrain) <- "ActivityNum"
colnames(activityTest) <- "ActivityNum"
mergedDataTrain <- cbind(subjectTrain, activityTrain, dataTrain)
mergedDataTest <- cbind(subjectTest, activityTest, dataTest)
mergedData <- rbind(mergedDataTrain, mergedDataTest)
mergedData <- arrange(mergedData, Subject, ActivityNum)
colnames(mergedData)
```

```
##  [1] "Subject"     "ActivityNum" "V1"          "V2"          "V3"         
##  [6] "V4"          "V5"          "V6"          "V41"         "V42"        
## [11] "V43"         "V44"         "V45"         "V46"         "V81"        
## [16] "V82"         "V83"         "V84"         "V85"         "V86"        
## [21] "V121"        "V122"        "V123"        "V124"        "V125"       
## [26] "V126"        "V161"        "V162"        "V163"        "V164"       
## [31] "V165"        "V166"        "V201"        "V202"        "V214"       
## [36] "V215"        "V227"        "V228"        "V240"        "V241"       
## [41] "V253"        "V254"        "V266"        "V267"        "V268"       
## [46] "V269"        "V270"        "V271"        "V345"        "V346"       
## [51] "V347"        "V348"        "V349"        "V350"        "V424"       
## [56] "V425"        "V426"        "V427"        "V428"        "V429"       
## [61] "V503"        "V504"        "V516"        "V517"        "V529"       
## [66] "V530"        "V542"        "V543"
```

The third to the last columns have column names being V1, V2, ...
These names are the indices of the mean() and std() data. 

## Process the merged data

Melt the data from the short wide format to the long thin format. Name the "V1, V2, ..." column as "FeatureCode"


```r
finalData <- melt(mergedData, id.vars = c("Subject", "ActivityNum"), variable.name = "FeatureCode")
```

Remove the "V" from column FeatureCode, and convert this column to numeric.


```r
finalData$FeatureCode <- as.numeric(sub("V", "", finalData$FeatureCode))
```

Add the feature names and the activity names.


```r
features2 <- features
colnames(features2) <- c("FeatureCode", "FeatureName")
finalData <- left_join(finalData, features2, by = "FeatureCode")
colnames(activityLabels) <- c("ActivityNum", "ActivityName")
finalData <- left_join(finalData, activityLabels, by = "ActivityNum")
finalData <- select(finalData, Subject, ActivityNum, ActivityName, FeatureCode, FeatureName, value)
head(finalData)
```

```
##   Subject ActivityNum ActivityName FeatureCode       FeatureName     value
## 1       1           1      WALKING           1 tBodyAcc-mean()-X 0.2820216
## 2       1           1      WALKING           1 tBodyAcc-mean()-X 0.2558408
## 3       1           1      WALKING           1 tBodyAcc-mean()-X 0.2548672
## 4       1           1      WALKING           1 tBodyAcc-mean()-X 0.3433705
## 5       1           1      WALKING           1 tBodyAcc-mean()-X 0.2762397
## 6       1           1      WALKING           1 tBodyAcc-mean()-X 0.2554682
```

Go through FeatureName and identify all the elements. 

```r
grepls <- function (text) {
  grepl(text, finalData$FeatureName)
}
finalData$Domain <- factor(grepls("^t") + grepls("^f") * 2, labels = c("time","frequency"))
finalData$BodyOrGravity <- factor(grepls("Body") + grepls("Gravity") * 2, labels = c("body", "gravity"))
finalData$Instrument <- factor(grepls("Acc") + grepls("Gyro") * 2, labels = c("accelerometer", "gyroscope"))
finalData$IsJerk <- grepls("Jerk")
finalData$IsMagnitude <- grepls("Mag")
finalData$MeanOrStd <- factor(grepls("mean\\(\\)") + grepls("std\\(\\)") * 2, labels = c("mean", "std"))
finalData$Axis <- factor(grepls("-X") + grepls("-Y") * 2 + grepls("-Z") * 3 + (!grepls("-X|-Y|-Z")) * 4, labels = c("X", "Y", "Z", "nonAxis"))
```

Move the "value" column to the last column.


```r
finalData <- select(finalData, -value, value)
head(finalData)
```

```
##   Subject ActivityNum ActivityName FeatureCode       FeatureName Domain
## 1       1           1      WALKING           1 tBodyAcc-mean()-X   time
## 2       1           1      WALKING           1 tBodyAcc-mean()-X   time
## 3       1           1      WALKING           1 tBodyAcc-mean()-X   time
## 4       1           1      WALKING           1 tBodyAcc-mean()-X   time
## 5       1           1      WALKING           1 tBodyAcc-mean()-X   time
## 6       1           1      WALKING           1 tBodyAcc-mean()-X   time
##   BodyOrGravity    Instrument IsJerk IsMagnitude MeanOrStd Axis     value
## 1          body accelerometer  FALSE       FALSE      mean    X 0.2820216
## 2          body accelerometer  FALSE       FALSE      mean    X 0.2558408
## 3          body accelerometer  FALSE       FALSE      mean    X 0.2548672
## 4          body accelerometer  FALSE       FALSE      mean    X 0.3433705
## 5          body accelerometer  FALSE       FALSE      mean    X 0.2762397
## 6          body accelerometer  FALSE       FALSE      mean    X 0.2554682
```

```r
tail(finalData)
```

```
##        Subject ActivityNum ActivityName FeatureCode
## 679729      30           6       LAYING         543
## 679730      30           6       LAYING         543
## 679731      30           6       LAYING         543
## 679732      30           6       LAYING         543
## 679733      30           6       LAYING         543
## 679734      30           6       LAYING         543
##                       FeatureName    Domain BodyOrGravity Instrument
## 679729 fBodyBodyGyroJerkMag-std() frequency          body  gyroscope
## 679730 fBodyBodyGyroJerkMag-std() frequency          body  gyroscope
## 679731 fBodyBodyGyroJerkMag-std() frequency          body  gyroscope
## 679732 fBodyBodyGyroJerkMag-std() frequency          body  gyroscope
## 679733 fBodyBodyGyroJerkMag-std() frequency          body  gyroscope
## 679734 fBodyBodyGyroJerkMag-std() frequency          body  gyroscope
##        IsJerk IsMagnitude MeanOrStd    Axis      value
## 679729   TRUE        TRUE       std nonAxis -0.9974532
## 679730   TRUE        TRUE       std nonAxis -0.9979687
## 679731   TRUE        TRUE       std nonAxis -0.9990995
## 679732   TRUE        TRUE       std nonAxis -0.9991540
## 679733   TRUE        TRUE       std nonAxis -0.9985502
## 679734   TRUE        TRUE       std nonAxis -0.9988617
```

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject


```r
tidyData <- finalData %>% group_by(Subject, ActivityName, Domain, BodyOrGravity, Instrument, IsJerk, IsMagnitude, MeanOrStd, Axis) %>% summarise(count = n(), average = mean(value))
head(tidyData)
```

```
## Source: local data frame [6 x 11]
## Groups: Subject, ActivityName, Domain, BodyOrGravity, Instrument, IsJerk, IsMagnitude, MeanOrStd [2]
## 
##   Subject ActivityName Domain BodyOrGravity    Instrument IsJerk
##     <int>       <fctr> <fctr>        <fctr>        <fctr>  <lgl>
## 1       1       LAYING   time          body accelerometer  FALSE
## 2       1       LAYING   time          body accelerometer  FALSE
## 3       1       LAYING   time          body accelerometer  FALSE
## 4       1       LAYING   time          body accelerometer  FALSE
## 5       1       LAYING   time          body accelerometer  FALSE
## 6       1       LAYING   time          body accelerometer  FALSE
## # ... with 5 more variables: IsMagnitude <lgl>, MeanOrStd <fctr>,
## #   Axis <fctr>, count <int>, average <dbl>
```

```r
tail(tidyData)
```

```
## Source: local data frame [6 x 11]
## Groups: Subject, ActivityName, Domain, BodyOrGravity, Instrument, IsJerk, IsMagnitude, MeanOrStd [5]
## 
##   Subject     ActivityName    Domain BodyOrGravity Instrument IsJerk
##     <int>           <fctr>    <fctr>        <fctr>     <fctr>  <lgl>
## 1      30 WALKING_UPSTAIRS frequency          body  gyroscope  FALSE
## 2      30 WALKING_UPSTAIRS frequency          body  gyroscope  FALSE
## 3      30 WALKING_UPSTAIRS frequency          body  gyroscope  FALSE
## 4      30 WALKING_UPSTAIRS frequency          body  gyroscope  FALSE
## 5      30 WALKING_UPSTAIRS frequency          body  gyroscope   TRUE
## 6      30 WALKING_UPSTAIRS frequency          body  gyroscope   TRUE
## # ... with 5 more variables: IsMagnitude <lgl>, MeanOrStd <fctr>,
## #   Axis <fctr>, count <int>, average <dbl>
```

## Writes the tidy data set


```r
write.table(tidyData, file = "tidyData.txt", quote = FALSE, row.names = FALSE)
```

