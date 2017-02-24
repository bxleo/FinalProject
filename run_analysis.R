# Final Project

## Preparation

#Load the libraries.

library(dplyr)
library(reshape2)

## Load the data

# Assume the data package has already been downloaded and unzipped in the working directory. Here we load the data.

path <- file.path(getwd(), "UCI HAR Dataset")
subjectTrain <- read.table(file.path(path, "train", "subject_train.txt"))
subjectTest  <- read.table(file.path(path, "test" , "subject_test.txt" ))
activityTrain <- read.table(file.path(path, "train", "y_train.txt"))
activityTest <- read.table(file.path(path, "test", "y_test.txt"))
dataTrain <- read.table(file.path(path, "train", "x_train.txt"))
dataTest <- read.table(file.path(path, "test", "x_test.txt"))
features <- read.table(file.path(path, "features.txt"))
activityLabels <- read.table(file.path(path, "activity_labels.txt"))


## Extract only the mean() and std() data

#Get the column indices of the mean() and std() data, and extract these data from the training and the test sets.

colnames(features) <- c("num", "name")
index <- features$num[grep("mean\\(\\)|std\\(\\)", features$name)]
dataTrain <- dataTrain[, index]
dataTest <- dataTest[, index]

## Merge the training and test data

#Merge the training and test sets, and add the subject and activity values.

colnames(subjectTrain) <- "Subject"
colnames(subjectTest) <- "Subject"
colnames(activityTrain) <- "ActivityNum"
colnames(activityTest) <- "ActivityNum"
mergedDataTrain <- cbind(subjectTrain, activityTrain, dataTrain)
mergedDataTest <- cbind(subjectTest, activityTest, dataTest)
mergedData <- rbind(mergedDataTrain, mergedDataTest)
mergedData <- arrange(mergedData, Subject, ActivityNum)
colnames(mergedData)

#The third to the last columns have column names being V1, V2, ...
#These names are the indices of the mean() and std() data. 

## Process the merged data

#Melt the data from the short wide format to the long thin format. Name the "V1, V2, ..." column as "FeatureCode"

finalData <- melt(mergedData, id.vars = c("Subject", "ActivityNum"), variable.name = "FeatureCode")

# Remove the "V" from column FeatureCode, and convert this column to numeric.

finalData$FeatureCode <- as.numeric(sub("V", "", finalData$FeatureCode))

#Add the feature names and the activity names.

features2 <- features
colnames(features2) <- c("FeatureCode", "FeatureName")
finalData <- left_join(finalData, features2, by = "FeatureCode")
colnames(activityLabels) <- c("ActivityNum", "ActivityName")
finalData <- left_join(finalData, activityLabels, by = "ActivityNum")
finalData <- select(finalData, Subject, ActivityNum, ActivityName, FeatureCode, FeatureName, value)
head(finalData)

#Go through FeatureName and identify all the elements. 
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

#Move the "value" column to the last column.

finalData <- select(finalData, -value, value)
head(finalData)
tail(finalData)

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject

tidyData <- finalData %>% group_by(Subject, ActivityName, Domain, BodyOrGravity, Instrument, IsJerk, IsMagnitude, MeanOrStd, Axis) %>% summarise(count = n(), average = mean(value))
head(tidyData)
tail(tidyData)

## Writes the tidy data set

write.table(tidyData, file = "tidyData.txt", quote = FALSE, row.names = FALSE)
