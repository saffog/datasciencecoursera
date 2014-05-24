# Script that runs the analysis required by Getting and Cleaning Data course
# If you want to skyp the dowload part, please download in working directory
# with the name: UCI HAR Dataset.zip, put inside "data" directory and unzip
# as "UCI HAR Dataset" directory.
# This script uses one auxiliar function CreateSpecificDataSet.
# At the end, the script is going to write the tidy data in tidyData.txt

# Getting and Cleaning Data Peer Assesment instructions:

#You should create one R script called run_analysis.R that does the following. 

#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive activity names. 
#5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#AUXILIAR FUNCTIONS
# CreateSpecificDataSet, Auxiliar function that create the function to load dataset from the UCI HAR Dataset and that cache common data.
# Input: 
#   datasetPath where is located the features or activyt_labels.
# Output: 
#   SpecificDataSet, function to load specific dataset
CreateSpecificDataSet <- function(datasetPath)
{

  print("Reading features.txt");
  # Features name, correspond to each column of the dataset.
  featuresNamesFilePath <- file.path(datasetPath,'features.txt');
  featuresNames <- read.table(featuresNamesFilePath);
  names(featuresNames) <- c('featureid','featurename');
  
  print("Reading activity_labels.txt");
  # Activity labels, table associating an id with a human readable name.
  activityLabelsFilePath <- file.path(datasetPath, 'activity_labels.txt');
  activityLabels <- read.table(activityLabelsFilePath);
  names(activityLabels) <- c('activityid','activitylabel');
  
  
  # SpecificDataSet, load a specific dataset from the UCI HAR Dataset.
  # Input: 
  #   setname, the name of the set to load: 'train' or 'test'
  # Output: 
  #   dataSet, the dataset loaded from X_test.txt with the additional data provided to identify the features, activities and subjects.
  SpecificDataSet <- function(setname)
  {      
    print(paste("SpecificDataSet, getting setname :",setname));
    
    # Loading the subject ids matching each row of dataset
    subjectsFilePath <- file.path(datasetPath, setname, paste('subject_',setname,'.txt', sep=''));
    subjectsIds <- read.table(subjectsFilePath);
    
    # Loading the activity ids matching each row of the dataset
    trainingActivityIdsFilePath <- file.path(datasetPath,setname,paste('y_',setname,'.txt', sep=''));
    trainingActivityIds <- read.table(trainingActivityIdsFilePath);
    
    # Loading the main part of the dataset
    dataSetFilePath <- file.path(datasetPath, setname, paste('X_',setname,'.txt', sep=''));
    dataSet <- read.table(dataSetFilePath);
    
    # Adding the columns with activities labels
    # A factor is created to match the training activyt ids with their names.
    dataSet <- cbind(dataSet,factor(trainingActivityIds[[1]],levels=activityLabels$activityid,labels=activityLabels$activitylabel));
    
    ## Adding the subjects ids
    ## A factor is created in order to keep the logic of ids and not numbers.
    dataSet <- cbind(dataSet,factor(subjectsIds[[1]]));
    
    ## Naming correctly the columns with the features names and the additional columns
    names(dataSet) <- c(as.character(featuresNames$featurename),'activity','subject');
    
    dataSet
  }
  
  # Return the function loading the specific data set
  SpecificDataSet
}

#Download data and unzip
dataPath<-"data";
dataSetPath<-file.path(dataPath,"UCI HAR Dataset");
zipData<-"UCI HAR Dataset.zip";
zipDataPath<-file.path(dataPath,zipData);
zipDataURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip";

if(!file.exists(dataPath))
{ 
  print(paste("Creating dir : ",dataPath));
  dir.create(dataPath); 
}
if(!file.exists(zipDataPath))
{ 
  print(paste("Downloading data in : ",zipDataPath));
  download.file(zipDataURL, zipDataPath, mode="wb");
}
if(!file.exists(dataSetPath)) 
{
  print(paste("unzip data in : ",zipDataPath));
  unzip(zipDataPath, exdir=dataPath)
}

#Create Specific Data Set
SpecificDataSet <- CreateSpecificDataSet(dataSetPath);

#Merge data set
print("Merging data set train with test");
mergedDataSet <- rbind(SpecificDataSet('train'),SpecificDataSet('test'));


#Using regular expresion filter data set, getting mean and std for each measurement.
print("Using regular expresion to filter data set by mean, std, activity and subject");
grepByMeanSTdActSub<-grep('(mean\\(\\)|std\\(\\))|^(activity|subject)$',names(mergedDataSet),ignore.case=T);
filteredDataSet <- mergedDataSet[grepByMeanSTdActSub];

#Finally we create the tidy data set
#We need reshape2 library
library(reshape2);
print("Melting filteredDataSet by subject and activity");
meltFilteredDataSet<-melt(filteredDataSet,id=c("subject","activity"));
print("Casting a data Frame using average of each variable by each subject and each activity");
tidyDataSet<-dcast(meltFilteredDataSet,subject+activity~variable,mean);

# Write the tidy dataset to tidy.txt
write.table(tidyDataSet,file="tidyData.txt");
print("Tidy Data Set was written to : tidyData.txt");