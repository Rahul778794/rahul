#########################################################################################
# This script is used to read data collected about human activity through smartphones   #
# and generate a comprehensive tidy dataset following instructions as described in      #
# Getting and Cleaning Data Course Final Project.                                       #
# More about the data collected here:                                                   #
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones   #
#########################################################################################

# Loading necessary libraries
library(dplyr)

# Dataset url and file name
datasetUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
datasetFile <- "UCI HAR Dataset.zip"
dataPath <- "UCI HAR Dataset"

# Only download and unzip file if not done before
if (!file.exists(datasetFile)) {
    download.file(datasetUrl, datasetFile, mode = "wb")
}
if (!file.exists(dataPath)) {
    unzip(datasetFile)
}

# Read necessary files about the training data and testing data
trainSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
trainValues<- read.table("./UCI HAR Dataset/train/X_train.txt")
trainActivities<- read.table("./UCI HAR Dataset/train/Y_train.txt")

testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
testValues <- read.table("./UCI HAR Dataset/test/X_test.txt")
testActivities<- read.table("./UCI HAR Dataset/test/Y_test.txt")

# read feature names and activity labels
featureNames <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
activityLabels <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activityLabels) <- c("id", "label")

# 1. Merge the training and the test sets to create one data set.
tidyDataset <- rbind(
    cbind(trainSubjects, trainValues, trainActivities),
    cbind(testSubjects, testValues, testActivities)
)
colnames(tidyDataset) <- c("subject", featureNames[, 2], "activity")

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
tidyDataset <- tidyDataset[, grepl("subject|activity|mean|std", colnames(tidyDataset))]

# 3. Use descriptive activity names to name the activities in the data set
tidyDataset$activity <- factor(tidyDataset$activity, levels = activityLabels[, 1], labels = activityLabels[, 2])

# 4. Appropriately label the data set with descriptive variable names. 
colnames(tidyDataset) <- gsub("^f", "Frequency", colnames(tidyDataset))
colnames(tidyDataset) <- gsub("^t", "Time", colnames(tidyDataset))
colnames(tidyDataset) <- gsub("Acc", "Accelerometer", colnames(tidyDataset))
colnames(tidyDataset) <- gsub("Gyro", "Gyroscope", colnames(tidyDataset))
colnames(tidyDataset) <- gsub("Mag", "Magnitude", colnames(tidyDataset))
colnames(tidyDataset) <- gsub("mean|Mean", "Mean", colnames(tidyDataset))
colnames(tidyDataset) <- gsub("Gravity", "Gravity", colnames(tidyDataset))
colnames(tidyDataset) <- gsub("std", "StandardDeviation", colnames(tidyDataset))
colnames(tidyDataset) <- gsub("[\\(\\)]", "", colnames(tidyDataset))

# 5. From the data set in step 4, create a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

tidyDataset%>%
    group_by(subject, activity)%>%
    summarize(across(-(1:2), mean))%>%
    write.table("tidyDatasetSummary.txt", row.name=FALSE)