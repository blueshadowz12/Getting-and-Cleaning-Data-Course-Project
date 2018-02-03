# Getting and Cleaning Data on R Programming

## Location for run_analysis.R
#This run_analysis script is part of UCI directory.
#It contains train and test folders.

setwd("E:\\R files\\Module 3")

## Setting up library packages
library(lubridate) #'dates
library(dplyr)     #'tables
library(ggplot2)   #'plots
library(tidyr)	   #'tidying data
library(plyr)
library(data.table)

## 1. Merging training and test sets to create one data set

## Reading activity rows

#Reading activity rows for train

activitytrain<-read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE)

#Reading activity rows for test
activitytest <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

#Merging train and test data
activity <- rbind(activitytrain,activitytest)

#Check activity rows
dim(activity) #10299 rows

#Creating a columnname
colnames(activity) <- c("activityid")


##Reading subject rows

# Reading subject row for train
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# Reading subject row for test 
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# Merging subject train and test data
subject<-rbind(subjecttrain, subjecttest)

# Check the subject rows
dim(subject)

#Create a column name
colnames(subject) <- c("subject")


##Reading the featurelabels
featurelabels <- read.table("./UCI HAR Dataset/features.txt",header=FALSE, sep=" ")

##Check the feature label rows
dim(featurelabels)

##Create column names
colnames(featurelabels) <-c("featureid","featurename")


##Reading x train data
featurestrain <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)

##Reading x test data
featurestest <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
# Merging x train and test data
features <-rbind(featurestrain,featurestest)

# Checking the features rows
dim(features)


#Create combined dataset

colnames(features) <- make.names(featurelabels$featurename, unique=TRUE)

#Add data columns for activity, subject, features together
datacolumn <-cbind(activity, subject, features)

#Checking structure
dim(datacolumn)

##2. Extract only the measurements on the mean and standard deviation for each measurement.

# Create a new table containing only columns with subjects, activityid, mean and sd
datameansd <- select(datacolumn,matches("subject|activityid|mean|sd"))

# Check the rows with measurements
dim(datameansd)


##3. Uses descriptive activity names to name the activities in the data set

# Read the labels for the activities
activitylabels<-read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE, sep=" ")

# Check the first 6 rows of data for activity labels
head(activitylabels)

# Create column names for activity labels
colnames(activitylabels)<- c("activityid","activity")

# Add the activity label to the dataset using a merge on activityid
datacolumn <- merge(x=datameansd, y=activitylabels, by="activityid")

# Checking if merged correctly
unique(datacolumn[,c("activity")])

# Exclude the activityid field
datacolumn <- select(datacolumn, -activityid)

# Reorder the columns so that the dataset starts with subject and activity
datacolumn<-select(datacolumn, subject, activity, 2:87)

#Checking the rows
dim(datacolumn)

#Checking internal structure of data

str(datacolumn)

## 4. Appropriately labels the data set with descriptive variable names.

# Get the column names and make them unique
colnames <-colnames(datacolumn)
colnames <- make.names(colnames, unique=TRUE)

#Cleaning column names by replacing characters
colnamesclean<-gsub("-", " ", colnames) #Replace - with a space
colnamesclean<-gsub("\\.", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("\\  ", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("\\  ", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("\\  ", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("tBody", "Body", colnamesclean) #Remove the t
colnamesclean<-gsub("tGravity", "Gravity", colnamesclean) #Remove the t
colnamesclean<-gsub("fBody", "Body", colnamesclean) #Remove the f
colnamesclean<-gsub("BodyBody", "Body", colnamesclean) #Remove double Body
colnamesclean<-gsub("^\\s+|\\s+$", "", colnamesclean) #Strip leading and trailing spaces

#Recreating cleaned column names
colnames(datacolumn) <-colnamesclean

#Checking internal structure
str(datacolumn)

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a dataframe table
tidy <- tbl_df(datacolumn)

# Create unique column names, otherwise the summary will give errors
colnames(tidy) <- make.names(colnames(tidy) , unique=TRUE)

# Group the data by subject and activity
tidygroup <-group_by(tidy, subject, activity)

# Calculate the mean (dplyr)
tidymean <- summarise_all(tidygroup, funs(mean))

# Reapply the clean column names
colnames(tidymean) <- colnamesclean

# Check the first 6 rows and 6 columns
tidymean[1:6, 1:6]

#Creating tidy data set
write.table(tidymean, file="tidy.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=TRUE)