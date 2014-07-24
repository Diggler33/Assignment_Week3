#=================================================================================================================
# This code was written by Ben Palmer: 24th July 2014 as part of the Getting and Cleaning Data - Assignment
#=================================================================================================================
# The following requirements were specified:
#
# Requirements
# You should create one R script called run_analysis.R that does the following.
#   1.Merges the training and the test sets to create one data set.
#   2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#   3.Uses descriptive activity names to name the activities in the data set
#   4.Appropriately labels the data set with descriptive variable names. 
#   5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject
#=================================================================================================================
# Please review the 'READ ME' file for further information
#=================================================================================================================

#===============================  LOAD PACKAGES  ==============================#
#     PLYR PACKAGE - DDPLY USED TO EXTRACT DATA IN REQUIREMENT 5 
#==============================================================================#
library(plyr)
#==============================================================================#

#========================  SET REQUIRED FILE PATHS ============================#

lookupfiles     <-  "C:/Users/Ben/STUDY/Getting.and.Cleaning.Data/Assignment/UCI HAR Dataset"
testfiles       <-  "C:/Users/Ben/STUDY/Getting.and.Cleaning.Data/Assignment/UCI HAR Dataset/test"
trainfiles      <-  "C:/Users/Ben/STUDY/Getting.and.Cleaning.Data/Assignment/UCI HAR Dataset/train"
assignmentroot  <-  "C:/Users/Ben/STUDY/Getting.and.Cleaning.Data/Assignment"

#========================  SET REQUIRED FILE NAMES ============================#

activity_labels_file  <-  "activity_labels.txt"
features_file         <-  "features.txt"
x_test_file           <-  "X_test.txt"
y_test_file           <-  "Y_test.txt"
subject_test_file     <-  "subject_test.txt"
x_train_file          <-  "X_train.txt"
y_train_file          <-  "Y_train.txt"
subject_train_file    <-  "subject_train.txt"

results_output_file   <-  "Getting_and_Cleaning Data-Assignment.txt"

#==============================================================================#


#========================================
#set working directory for look-up files
#========================================
setwd(lookupfiles)


#===========================  LOAD LOOK_UP FILES  =============================#
#Read look-up files
activity_labels <- read.table(activity_labels_file,
                              header=FALSE,na.strings=TRUE,
                              stringsAsFactors=FALSE)

names(activity_labels) <- c("activitycode","activity") #Assign names to columns in activity label dataset

features <- read.table(features_file,header=FALSE,na.strings=TRUE) 
#==============================================================================#



#===========================  CLEAN COLUMN FEATURES DS  =======================#
# CLEAN FIELDS EXTRACTED TO FEATURES DATA SET.(TO BE USED AS COLUMN HEADERS)
#==============================================================================#
targetcols      <- grep("mean+\\(+\\)|-std+\\(+\\)",features$V2) #identify only std & mean cols
stdmeancols     <- features[targetcols,]                        # create new dataset with only required data
stdmeancols$V2  <- tolower(x=stdmeancols$V2)
stdmeancols$V2  <- gsub("\\(+\\)"," ",stdmeancols$V2,perl=TRUE)
stdmeancols$V2  <- gsub(pattern="acc",replacement="accelerometer",stdmeancols$V2,perl=TRUE)
stdmeancols$V2  <- gsub(pattern="gyro",replacement="gyroscope",stdmeancols$V2,perl=TRUE)
stdmeancols$V2  <- gsub("-","",stdmeancols$V2)
stdmeancols$V2  <- gsub("mean"," mean",stdmeancols$V2)
stdmeancols$V2  <- gsub("mean\\s+$"," mean ",stdmeancols$V2)
stdmeancols$V2  <- gsub("std"," std",stdmeancols$V2)
stdmeancols$V2  <- gsub("std\\s+$"," std ",stdmeancols$V2)
stdmeancols$V2  <- gsub("\\s+std","std",stdmeancols$V2,perl=TRUE)
stdmeancols$V2  <- gsub("\\s+mean","mean",stdmeancols$V2,perl=TRUE)
stdmeancols$V2  <- gsub("^t","time",stdmeancols$V2)
stdmeancols$V2  <- gsub("^f","frequency",stdmeancols$V2)
stdmeancols$V2  <- gsub("bodybody","body",stdmeancols$V2)
stdmeancols$V2  <- gsub(" x","ofx",stdmeancols$V2)
stdmeancols$V2  <- gsub(" y","ofy",stdmeancols$V2)
stdmeancols$V2  <- gsub(" z","ofz",stdmeancols$V2)
stdmeancols$V2  <- gsub("std","standarddeviation",stdmeancols$V2)
#==============================================================================#

#========================================
#Change working directory for test files
#========================================
setwd(testfiles)
#=============================  LOAD TEST FILES  ==============================#
test.set <- read.table(x_test_file,header=FALSE,na.strings=TRUE) #Test Set data (Headers are contained in Y_test file)
test.set <- test.set[,targetcols] #USING THE TARGET COLS DATA SET TO ONLY USE REQUIRED COLUMNS (MEAN & STD)
test.labels <- read.table(y_test_file) 
names(test.labels)<- "activitycode"
subject.test <- read.table(subject_test_file,header=FALSE,na.strings=TRUE,sep=",") 

# Assign column headers to test data set from cleaned stdmeancols 
names(test.set) <- stdmeancols$V2
#==============================================================================#


#========================================
#Change working directory for train files
#========================================
setwd(trainfiles)


#=============================  LOAD TRAIN FILES  =============================#
train.set <- read.table(x_train_file,header=FALSE,na.strings=TRUE) #train Set data 
train.set <- train.set[,targetcols]
train.labels <- read.table(y_train_file) #train Label data
names(train.labels)<- "activitycode"
subject.train <- read.table(subject_train_file,header=FALSE,na.strings=TRUE,sep=",") #Subject train data

names(train.set) <- stdmeancols$V2
#==============================================================================#


#=============================  MERGE DATA SETS  ==============================#

# Add y data(Label) to test dataset
activity.code <- as.vector(test.labels[,1])
test.set$activitycode <- activity.code
test.set$activity <- as.vector(activity_labels$activity[activity.code])

# Add y data(Label) to train dataset
activity.code <- as.vector(train.labels[,1])
train.set$activitycode <- activity.code
train.set$activity <- as.vector(activity_labels$activity[activity.code])

# Add subject data to test dataset
subject.code <- as.vector(subject.test[,1])
test.set$subjectcode <- subject.code

# Add subject data to train dataset
subject.code <- as.vector(subject.train[,1])
train.set$subjectcode <- subject.code

# Merge test dataset with train dataset 
master.data.set <- rbind(test.set,train.set)
#==============================================================================#


#======================  CREATE CLEAN AGGREGATED DATASET  =====================#
result <- ddply(master.data.set, c("subjectcode","activity"), numcolwise(mean))
#==============================================================================#

#============================================
#Change working directory for Assignment root
#============================================
setwd(assignmentroot)


#========================  WRITE RESULTS TO TEXT FILE  ========================#
write.table(result,results_output_file)
#==============================================================================#

#======================== CODE USED TO VERIFY RESULTS FILE ====================#

testing_outputs <- train.set <- read.table(results_output_file,header=TRUE,na.strings=TRUE) #train Set data 
View(testing_outputs)

#==============================================================================#



#===== CLEAN UP======
rm(list=ls())
#====================
