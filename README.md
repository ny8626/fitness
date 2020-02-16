# Fitnessdata

This repo have a R script called run_analysis.R that create a tidy data set that is required to generate in step 5 of the Week4 Peer-graded Assignment of Getting and Cleaning Data. 

## Overview of Week4 Peer-graded Assignment of Getting and Cleaning Data
One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

### Original Data Sets
Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

### Script and Data Set Location
The R script called run_analysis.R need to be placed in the main directory that can be run as long as the Samsung data is in /data directory right under your working directory.

### What does the R script called run_analysis.R?

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Detai of the Human Activity Recognition Using Smartphones Dataset Version 1.0
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Universit・degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we 
captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly 
partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled 
in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, 
which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body 
acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore 
a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating 
variables from the time and frequency domain. See 'features_info.txt' for more details. 

### For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ 
and tGyro-XYZ. 

These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. 
Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency 
of 20 Hz to remove noise. 

Similarly, the acceleration signal was then separated into body and gravity acceleration 
signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals 
(tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using 
the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing 
fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. 
(Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

## Code Book of the tidy data set that is genereated by the uploaded run_analysis.R:


* Personnel : Unique index of 30 volunteers
* ActivityNm : 6 activities: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING STANDING and LAYING
* tBodyAcc-mean()-X : Mean value of body X-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tBodyAcc-mean()-Y : Mean value of body Y-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tBodyAcc-mean()-Z : Mean value of body Z-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tBodyAcc-std()-X : Standard deviation of body X-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tBodyAcc-std()-Y : Standard deviation of body Y-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tBodyAcc-std()-Z : Standard deviation of body Z-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tGravityAcc-mean()-X : Mean value of gravity X-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tGravityAcc-mean()-Y : Mean value of gravity Y-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tGravityAcc-mean()-Z : Mean value of gravity Z-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tGravityAcc-std()-X : Standard deviation of gravity X-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tGravityAcc-std()-Y : Standard deviation of gravity Y-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tGravityAcc-std()-Z : Standard deviation of gravity Z-axial acceleration signals which is separated from the accelerometerand gyroscope 3-axial raw signals.
* tBodyAccJerk-mean()-X :Mean value of the body X-axial linear acceleration were derived in time to obtain Jerk signals. 
* tBodyAccJerk-mean()-Y :Mean value of the body Y-axial linear acceleration were derived in time to obtain Jerk signals. 
* tBodyAccJerk-mean()-Z :Mean value of the body Z-axial linear acceleration were derived in time to obtain Jerk signals. 
* tBodyAccJerk-std()-X : Standard deviation of the body X-axial linear acceleration were derived in time to obtain Jerk signals. 
* tBodyAccJerk-std()-Y : Standard deviation of the body Y-axial linear acceleration were derived in time to obtain Jerk signals.  
* tBodyAccJerk-std()-Z : Standard deviation of the body Z-axial linear acceleration were derived in time to obtain Jerk signals. 
* tBodyGyro-mean()-X : Mean value of X-axial gyroscope 3-axial raw signals
* tBodyGyro-mean()-Y : Mean value of Y-axial gyroscope 3-axial raw signals
* tBodyGyro-mean()-Z : Mean value of Z-axial gyroscope 3-axial raw signals
* tBodyGyro-std()-X : Standard deviation of X-axial gyroscope 3-axial raw signals
* tBodyGyro-std()-Y : Standard deviation of Y-axial gyroscope 3-axial raw signals
* tBodyGyro-std()-Z : Standard deviation of Z-axial gyroscope 3-axial raw signals
* tBodyGyroJerk-mean()-X : Mean value of the body X-axial linear angular velocity were derived in time to obtain Jerk signals.
* tBodyGyroJerk-mean()-Y : Mean value of the body Y-axial linear angular velocity were derived in time to obtain Jerk signals.
* tBodyGyroJerk-mean()-Z : Mean value of the body Z-axial linear angular velocity were derived in time to obtain Jerk signals.
* tBodyGyroJerk-std()-X : Standard deviation of the body X-axial linear angular velocity were derived in time to obtain Jerk signals.
* tBodyGyroJerk-std()-Y : Standard deviation of the body Y-axial linear angular velocity were derived in time to obtain Jerk signals.
* tBodyGyroJerk-std()-Z : Standard deviation of the body Z-axial linear angular velocity were derived in time to obtain Jerk signals.
* tBodyAccMag-mean() : Mean value of calculated magnitude of the tBodyAcc three-dimensional signals by using the Euclidean norm
* tBodyAccMag-std() : Standard deviation of calculated magnitude of the tBodyAcc three-dimensional signals by using the Euclidean norm
* tGravityAccMag-mean() : Mean value of calculated magnitude of the tGravityAcc three-dimensional signals by using the Euclidean norm
* tGravityAccMag-std() : Standard deviation of calculated magnitude of the tGravityAcc three-dimensional signals by using the Euclidean norm
* tBodyAccJerkMag-mean() : Mean value of calculated magnitude of the tBodyAccJerk three-dimensional signals by using the Euclidean norm
* tBodyAccJerkMag-std() : Standard deviation of calculated magnitude of the tBodyAccJerk three-dimensional signals by using the Euclidean norm
* tBodyGyroMag-mean() : Mean value of calculated magnitude of the tBodyGyro three-dimensional signals by using the Euclidean norm
* tBodyGyroMag-std() : Standard deviation of calculated magnitude of the tBodyGyro three-dimensional signals by using the Euclidean norm
* tBodyGyroJerkMag-mean() : Mean value of calculated magnitude of the tBodyGyroJerk three-dimensional signals by using the Euclidean norm
* tBodyGyroJerkMag-std() : Standard deviation of calculated magnitude of the tBodyGyroJerk three-dimensional signals by using the Euclidean norm
* fBodyAcc-mean()-X : Mean value of tBodyAcc X-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAcc-mean()-Y : Mean value of tBodyAcc Y-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAcc-mean()-Z : Mean value of tBodyAcc Z-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAcc-std()-X : Standard deviation of tBodyAcc X-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAcc-std()-Y : Standard deviation of tBodyAcc Y-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAcc-std()-Z : Standard deviation of tBodyAcc Z-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAcc-meanFreq()-X : Weighted average of the X-axial fBodyAcc frequency components.
* fBodyAcc-meanFreq()-Y : Weighted average of the Y-axial fBodyAcc frequency components.
* fBodyAcc-meanFreq()-Z : Weighted average of the Z-axial fBodyAcc frequency components.
* fBodyAccJerk-mean()-X : Mean value of tBodyAccJerk X-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAccJerk-mean()-Y : Mean value of tBodyAccJerk Y-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAccJerk-mean()-Z : Mean value of tBodyAccJerk Z-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAccJerk-std()-X : Standard deviation of tBodyAccJerk X-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAccJerk-std()-Y : Standard deviation of tBodyAccJerk Y-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAccJerk-std()-Z : Standard deviation of tBodyAccJerk Z-axial which is applied by Fast Fourier Transform (FFT).
* fBodyAccJerk-meanFreq()-X : Weighted average of the X-axial fBodyAccJerk frequency components.
* fBodyAccJerk-meanFreq()-Y : Weighted average of the Y-axial fBodyAccJerk frequency components.
* fBodyAccJerk-meanFreq()-Z : Weighted average of the Z-axial fBodyAccJerk frequency components.
* fBodyGyro-mean()-X : Mean value of tBodyGyro X-axial which is applied by Fast Fourier Transform (FFT).
* fBodyGyro-mean()-Y : Mean value of tBodyGyro Y-axial which is applied by Fast Fourier Transform (FFT).
* fBodyGyro-mean()-Z : Mean value of tBodyGyro Z-axial which is applied by Fast Fourier Transform (FFT).
* fBodyGyro-std()-X : Standard deviation of tBodyGyro X-axial which is applied by Fast Fourier Transform (FFT).
* fBodyGyro-std()-Y : Standard deviation of tBodyGyro Y-axial which is applied by Fast Fourier Transform (FFT).
* fBodyGyro-std()-Z : Standard deviation of tBodyGyro Z-axial which is applied by Fast Fourier Transform (FFT).
* fBodyGyro-meanFreq()-X : Weighted average of the X-axial fBodyGyro frequency components.
* fBodyGyro-meanFreq()-Y : Weighted average of the Y-axial fBodyGyro frequency components.
* fBodyGyro-meanFreq()-Z : Weighted average of the Z-axial fBodyGyro frequency components.
* fBodyAccMag-mean() : Mean value of tBodyAccMag which is applied by Fast Fourier Transform (FFT).
* fBodyAccMag-std() : Standard deviation of tBodyAccMag which is applied by Fast Fourier Transform (FFT).
* fBodyAccMag-meanFreq() : Weighted average of the fBodyAccMag frequency components.
* fBodyBodyAccJerkMag-mean() : Mean value of tBodyBodyAccJerkMag which is applied by Fast Fourier Transform (FFT).
* fBodyBodyAccJerkMag-std() : Standard deviation of tBodyBodyAccJerkMag which is applied by Fast Fourier Transform (FFT).
* fBodyBodyAccJerkMag-meanFreq() : Weighted average of the fBodyBodyAccJerkMag frequency components.
* fBodyBodyGyroMag-mean() : Mean value of tBodyBodyGyroMag which is applied by Fast Fourier Transform (FFT).
* fBodyBodyGyroMag-std() : Standard deviation of tBodyBodyGyroMag which is applied by Fast Fourier Transform (FFT).
* fBodyBodyGyroMag-meanFreq() : Weighted average of the fBodyBodyGyroMag frequency components.
* fBodyBodyGyroJerkMag-mean() : Mean value of tBodyBodyGyroJerkMag which is applied by Fast Fourier Transform (FFT).
* fBodyBodyGyroJerkMag-std() : Standard deviation tBodyBodyGyroJerkMag which is applied by Fast Fourier Transform (FFT).
* fBodyBodyGyroJerkMag-meanFreq() : Weighted average of the fBodyBodyGyroJer frequency components.

End
