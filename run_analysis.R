##########################################################################################################

## Coursera: Getting and Cleaning Data Course Project
## Jorge Gonzalez
## August 2014

# run_Analysis.R File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################
# assume datasets are downloaded and unziped in "Project/UCI HAR Dataset" folder

setwd ("C:/COURSERA/Getdata/Project")

# Install and load libraries

if (!require("data.table")) { 
           install.packages("data.table") 
         }  

if (!require("reshape2")) { 
           install.packages("reshape2") 
         } 

require("data.table") 
require("reshape2") 

#Step 1: "Merges the training and the test sets to create one data set." 


 # read subject training data 
 subject_train = read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("subject_id")) 
 # assign row number as the values of ID column 
 subject_train$ID <- as.numeric(rownames(subject_train)) 
 # read training data set
 X_train = read.table("UCI HAR Dataset/train/X_train.txt") 
 # assign row number as the values of ID column 
 X_train$ID <- as.numeric(rownames(X_train)) 
 # read activity training data 
 y_train = read.table("UCI HAR Dataset/train/y_train.txt", col.names=c("activity_id"))  # max = 6 
 # assign row number as the values of ID column 
 y_train$ID <- as.numeric(rownames(y_train)) 
 # merge subject_train and y_train to train 
 train <- merge(subject_train, y_train, all=TRUE) 
 # merge train and X_train 
 train <- merge(train, X_train, all=TRUE) 
 

 # read subject test data 
 subject_test = read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject_id")) 
 # assign row number as the values of ID column 
 subject_test$ID <- as.numeric(rownames(subject_test)) 
 # read testing data set
 X_test = read.table("UCI HAR Dataset/test/X_test.txt") 
 # assign row number as the values of ID column 
 X_test$ID <- as.numeric(rownames(X_test)) 
 # read activity testing data 
 y_test = read.table("UCI HAR Dataset/test/y_test.txt", col.names=c("activity_id"))  # max = 6 
 # assign row number as the values of ID column 
 y_test$ID <- as.numeric(rownames(y_test)) 
 # merge subject_test and y_test to train 
 test <- merge(subject_test, y_test, all=TRUE)  
 # merge test and X_test 
 test <- merge(test, X_test, all=TRUE)  
   
 #combine train and test 
 combtraintest <- rbind(train, test) 
 #
 #
#################################################################################################

#Step 2: "Extracts only the measurements on the mean and standard deviation for each measurement." 

 features = read.table("UCI HAR Dataset/features.txt", col.names=c("feature_id", "feature_label"),)  #561 
 #Extracts only the measurements on the mean and standard deviation for each measurement.  
 selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ] 
 meanandSD <- combtraintest[, c(c(1, 2, 3), selected_features$feature_id + 3) ] 
#
#
#################################################################################################

#Step 3: "Uses descriptive activity names to name the activities in the data set." 

 activity_labels = read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_label"),) # 
 meanandSDActNames = merge(meanandSD, activity_labels) 
 #
 #
 ################################################################################################

#Step 4: "Appropriately labels the data set with descriptive activity names."  

 selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label) 
 selected_features$feature_label = gsub("-", ".", selected_features$feature_label) 
 for (i in 1:length(selected_features$feature_label)) { 
             colnames(meanandSDActNames)[i + 3] <- selected_features$feature_label[i] 
         } 

meanandSDActNamesFeautres = meanandSDActNames 
#
#
###############################################################################################

 #Step 5: "Creates a second, independent tidy data set with the average of each variable for each activity and each subject." 

 drops <- c("ID","activity_label") 
 dataset5 <- meanandSDActNamesFeautres[,!(names(meanandSDActNamesFeautres) %in% drops)] 
 aggdata <-aggregate(dataset5, by=list(subject = dataset5$subject_id, activity = dataset5$activity_id), FUN=mean, na.rm=TRUE) 
 drops <- c("subject","activity") 
 aggdata <- aggdata[,!(names(aggdata) %in% drops)] 
 aggdata = merge(activity_labels,aggdata) 
 write.table(aggdata, file="tidydataset.txt", sep="\t", row.names=TRUE)
# If you want to create a csv file, uncomment the next row and run it in R
#write.csv(file="tidydataset.csv", x=aggdata) 
# 
###############################################################################################
# End of script