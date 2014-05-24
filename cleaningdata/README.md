#Getting and Cleaning Data Peer Assessment project

The goal is to tidy the data set from the Human Activity Recognition Using [Smartphones Dataset](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) following those instructions:

	* Merges the training and the test sets to create one data set.
	* Extracts only the measurements on the mean and standard deviation for each measurement. 
	* Uses descriptive activity names to name the activities in the data set
	* Appropriately labels the data set with descriptive activity names. 
	* Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Please take a look into CodeBook.md for full explaination of original data set and transformed tidy data set.

The data set can be found at the following URL: [Smartphones Dataset](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

##Usage

```R
  source('run_analysis.R')
```

If data is not present, the script will download the dataset from UCI HAR Dataset in the 'data' subfolder and unzip it.

The script will create a file: **'tidyData.txt'**.


##References:

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012