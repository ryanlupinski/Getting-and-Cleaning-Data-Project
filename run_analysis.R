####################################################################################
# Getting and Cleaning Data Project Course Project
# Ryan Lupinski
# 6/15/15
#
# File Description:
#
# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

####################################################################################

# 1 Merges the training and the test sets to create one data set.

# Clean enviornment area
rm(list=ls())

# Create data frames from txt files
testX  <- read.table("./test/X_test.txt",header=FALSE)
testY  <- read.table("./test/Y_test.txt",header=FALSE)
trainX <- read.table("./train/X_train.txt",header=FALSE)
trainY <- read.table("./train/Y_train.txt",header=FALSE)

# Create data frames from txt files
features       <- read.table('./features.txt',header=FALSE)
subjectTrain   <- read.table('./train/subject_train.txt',header=FALSE)
subjectTest    <- read.table('./test/subject_test.txt',header=FALSE)

# Assigin column names to the data
colnames(activityLabels)  <- c('activityId','activityType')
colnames(subjectTrain)    <- "subjectId"
colnames(subjectTest)     <- "subjectId"
colnames(trainX)          <- features[,2] 
colnames(trainY)          <- "activityId"
colnames(testX)           <- features[,2] 
colnames(testY)           <- "activityId"

# Create merged data frame of test, and train data
testData     <- cbind(testY,subjectTest,testX)
trainingData <- cbind(trainY,subjectTrain,trainX)

# Combine training and test data to create a final data set
finalData <- rbind(trainingData,testData)


# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  <- colnames(finalData)


# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_and_std <- finalData[,c(1,2,grep("std", colnames(finalData)), grep("mean", colnames(finalData)))]

# 3 Uses descriptive activity names to name the activities in the data set
activityLabels <- read.table('./activity_labels.txt',header=FALSE)
y_data <- rbind(testY,trainY)
finalData[,1] <- activityLabels[y_data[,1],2]

# 4 Appropriately labels the data set with descriptive variable names. 
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;


# 5 From the data set in step 4, creates a second, independent tidy data set with 
#   the average of each variable for each activity and each subject.
# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
