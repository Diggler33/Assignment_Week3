-----------------------------------------------------------------
Getting and Cleaning Data - Assignment  
Ben Palmer - July 2014  
README File  

-----------------------------------------------------------------


## Introduction


This repo has been created as part of the Getting and Cleaning Data Assignment. 

The following requirements were specified.


*You should create one R script called run_analysis.R that does the following.*

	1.Merges the training and the test sets to create one data set.
	2.Extracts only the measurements on the mean and standard deviation for each measurement. 
	3.Uses descriptive activity names to name the activities in the data set
	4.Appropriately labels the data set with descriptive variable names. 
	5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject


There are two files provided (Other than README):

	1. Code Book 
	   This File describes the variables, the data, and any transformations or work that were performed to clean up the data
	2. run_analysis.R
	   This code file meets the requirements specified above	


## Data used


The data has been sourced from the UCI Machine Learning Repository (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) and represents data collected from the accelerometers from the Samsung Galaxy S smartphone

The following excerpt from the 'README' file supplied with this data provides a summary.

"The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details."

The following files are used:

	activity_labels.txt
	features.txt
	X_test.txt
	Y_test.txt
	subject_test.txt
	X_train.txt
	Y_train.txt
	subject_train.txt


## Script 1 - Code Book 

The Code book has been created using the 'Features_info.txt' file supplied with the data. This provides detail of the source datasets. I have also added detail of the new fields created and transformations applied.

## Script 2 - run_analysis.R

The run_analysis.R file loads, cleans, merges and aggregates the data and then outputs the results to a text file performing the following steps.


1. Load Look up files (activity_labels.txt,features.txt)

2. Clean Features data (Used for column headers of main dataset) 

3. Limited all records within the features dataset where either mean() or std() is present

4. Load Test Files (X_test.txt,Y_test.txt,subject_test.txt) limiting to only columns matching features dataset

5. Load train Files (X_train.txt,Y_train.txt,subject_train.txt) limiting to only columns matching features dataset

6. Merge data sets

	* Added activity ID from Y_test to X_test.txt
	* Added descriptive activity field from activity_labels.txt to x_test.txt using activity ID
	* Added subject data from subject_test.txt to X_test.txt
	* Added activity ID from Y_train to X_train.txt
	* Added descriptive activity field from activity_labels.txt to x_train.txt using activity ID
	* Added subject data from subject_train.txt to X_train.txt
	* Merged X_test.txt with x_train to form a master data set

6. Create clean aggregated data set with the average of each variable for each activity and each subject using ddply from plyr package

7. Write outputs to text file (Getting_and_Cleaning Data-Assignment.txt)

