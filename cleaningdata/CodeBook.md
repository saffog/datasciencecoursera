# Code Book

##Objective

The current analysis have the purpose to convert the Human Activity Recognition using smartphones dataset into a Tidy Set.

##Original Data Set

For each record it is provided:

	- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
	- Triaxial Angular velocity from the gyroscope. 
	- A 561-feature vector with time and frequency domain variables. 
	- Its activity label. 
	- An identifier of the subject who carried out the experiment.

The dataset includes the following files:

	- 'README.txt'
	- 'features_info.txt': Shows information about the variables used on the feature vector.
	- 'features.txt': List of all features.
	- 'activity_labels.txt': Links the class labels with their activity name.
	- 'train/X_train.txt': Training set.
	- 'train/y_train.txt': Training labels.
	- 'test/X_test.txt': Test set.
	- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

	- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
	- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. 
	
Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

	- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
	- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes:

	- Features are normalized and bounded within [-1,1].
	- Each feature vector is a row on the text file.

Feature Selection 

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

	- tBodyAcc-XYZ
	- tGravityAcc-XYZ
	- tBodyAccJerk-XYZ
	- tBodyGyro-XYZ
	- tBodyGyroJerk-XYZ
	- tBodyAccMag
	- tGravityAccMag
	- tBodyAccJerkMag
	- tBodyGyroMag
	- tBodyGyroJerkMag
	- fBodyAcc-XYZ
	- fBodyAccJerk-XYZ
	- fBodyGyro-XYZ
	- fBodyAccMag
	- fBodyAccJerkMag
	- fBodyGyroMag
	- fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

	- mean(): Mean value
	- std(): Standard deviation
	- mad(): Median absolute deviation 
	- max(): Largest value in array
	- min(): Smallest value in array
	- sma(): Signal magnitude area
	- energy(): Energy measure. Sum of the squares divided by the number of values. 
	- iqr(): Interquartile range 
	- entropy(): Signal entropy
	- arCoeff(): Autorregresion coefficients with Burg order equal to 4
	- correlation(): correlation coefficient between two signals
	- maxInds(): index of the frequency component with largest magnitude
	- meanFreq(): Weighted average of the frequency components to obtain a mean frequency
	- skewness(): skewness of the frequency domain signal 
	- kurtosis(): kurtosis of the frequency domain signal 
	- bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
	- angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

	- gravityMean
	- tBodyAccMean
	- tBodyAccJerkMean
	- tBodyGyroMean
	- tBodyGyroJerkMean
	

##The analysis dataset


###Steps of the analysis:

Load the data set:

	* Load each dataset
	* Add the activity labels (using descriptive names) and the subjects to each dataset
	* Name the variable correctly
	* Merges the training and the test sets to create one data set.
	* Extracts only the measurements on the mean and standard deviation for each measurement.
	Export a tidy data set:
	* From the previously generated data set, create a second, independent tidy data set with the average of each variable for each activity and each subject.
	* Save the tidy data set in a file. ("tidyData.txt")


###Variables in the final dataset:

All the variables in the tidy data set are:

	* subject, the id of the person performing the test.
	* activity, contain the name of the activity produced. It is a factor with which the ids match the ids use in the original data set.
	* The mean of all the measures of the previsous data set (mean() and std() measures), aggregated by subject and activity.


##Reference:

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
[Human Activity Recognition...](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)