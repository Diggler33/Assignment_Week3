=================
Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

=======================
transformations applied
=======================

Names have been changed for readability to use the following convention

	All letters are lower case.
	prefix 't' replaced by 'time'
	prefix 'f' replaced with 'frequency'
	'Acc' replaced with accelerometer
	'Gyro' replaced with gyroscope
	'std' replaced with standard deviation

All of these fields are numeric to 9 decimals

	timebodyaccelerometer-XYZ
	timegravityaccelerometer-XYZ
	timebodyaccelerometerjerk-XYZ
	timebodygyroscope-XYZ
	timebodygyroscopejerk-XYZ
	timebodyaccelerometermag
	timegravityccelerometermag
	timebodyaccelerometerjerkmag
	timebodygyroscopemag
	timebodygyroscopejerkmag
	frequencybodyaccelerometer-XYZ
	frequencybodyaccelerometerJerk-XYZ
	frequencybodygyroscope-XYZ
	frequencybodyaccelerometermag
	frequencybodyaccelerometerjerkmag
	frequencybodygyroscopemag
	frequencybodygyroscopejerkmag


=======================
Extra fields
=======================

activitycode	Numeric 2
	Code assigned to the type of activity being performed
		1
		2
		3
		4

activity	Character 18
	Descriptive of activity being performed
		1. Walking
		2. Walking Upstairs
		3. Walking Downstairs
		4. Sitting
		5. Standing
		6. Laying
		
		
	
subjectcode 	Numeric 2
	Code assigned to subject performing the test (1 - 30)
