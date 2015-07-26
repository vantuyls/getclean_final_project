

# You should create one R script called run_analysis.R that does the following. 
## 1) Merges the training and the test sets to create one data set.
## 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive variable names. 
## 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)

# import files and rename file headers (where necessary)
# import here assumes the files are in the local directory

xtest <- read.table("x_test.txt")
xtrain <- read.table("x_train.txt")
ytest <- read.table("y_test.txt")
ytest <- rename(ytest, label = V1)
ytrain <- read.table("y_train.txt")
ytrain <- rename(ytrain, label = V1)
subjecttest <- read.table("subject_test.txt")
subjecttest <- rename(subjecttest, subject = V1)
subjecttrain <- read.table("subject_train.txt")
subjecttrain <- rename(subjecttrain, subject = V1)

activityLabels <- read.table("activity_labels.txt")


# merge common data types together into single data frame
trainingData <- cbind(subjecttrain, ytrain, xtrain)
trainingData <- mutate(trainingData, dataType = "train")
# reorder columns to put the data type in the first column
trainingData <- trainingData[c(564, 1:563)]

testData <- cbind(subjecttest, ytest, xtest)
testData <- mutate(testData, dataType = "test")
# reorder the columns to put the data type in the first column
testData <- testData[c(564, 1:563)]


# then merge all data into a single data frame
allData <- rbind(trainingData, testData)

# calculate mean and sd for each row (measurement) in the dataframe
measureMeans <- apply(allData[,4:564], 1, mean)
measureSDs <- apply(allData[4:564], 1, sd)

allData <- cbind(allData, measureMeans, measureSDs)

# extract only the relevant data from the allData dataframe
extractedData <- select(allData, dataType, subject, label, measureMeans, measureSDs)

# add activity labels to extracted dataset
labeledData <- merge(extractedData, activityLabels)

# reorder columns and remove label field
labeledData <- labeledData[,c(2, 6, 3, 4, 5)]

## Get the average of each variable by activity and subject

finalData <- aggregate(labeledData, by = list(labeledData$activity, labeledData$subject), FUN = mean)

# select final columns and rename
finalData <- rename(finalData, Activity = Group.1, Subject = Group.2)
finalData <- select(finalData, Activity, Subject, measureMeans, measureSDs)

# write final data to .csv in local directory
write.csv(finalData, "finalData.csv")


