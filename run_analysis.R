#Set Work directory
setwd("/Users/jaco/Documents/datasciencecoursera/Course 3_Getting and Cleaning Data/Week4/Assignment")


#Prework: Dowload, unzip and read files

  downloadfile <- "UCIdata.zip"
  file_url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  dir <- "UCI HAR Dataset"

  # File download verification. If file does not exist, download to working directory.
  if(!file.exists(downloadfile)){
    download.file(file_url,downloadfile, mode = "wb") 
  }

  # File unzip verification. If the directory does not exist, unzip the downloaded file.
  if(!file.exists(dir)){
   unzip("UCIdata.zip", files = NULL, exdir=".")
  }

  #Read and write data
  Subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  Subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  
  Y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  Y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  
  features <- read.table("UCI HAR Dataset/features.txt")[2]   #Read and write features data to list

  activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt") 
  
  
#1 Merges the training and the test sets to create one data set.

  #Merge records
  Subject_merged <- rbind(Subject_train, Subject_test)
  X_merged <- rbind(X_train, X_test)
  Y_merged <- rbind(Y_train, Y_test)

  #Rename single columns  
  names(Subject_merged)<-c("Subject")
  names(Y_merged)<- c("Activity")
  names(features) = c("Feature_name") 
  
  #Rename X_merged feature columns  
  names(X_merged)<- features$Feature_name
  
  #Merge columns
  Column_merge1 <- cbind(Subject_merged, Y_merged)
  Data_total <- cbind(X_merged, Column_merge1)

#2 Extracts only the measurements on the mean and standard deviation for each measurement.
  
  #Read and write mean and std dev features data to dataframes
  features_mean_std <- features$Feature_name[grepl("mean\\(\\)|std\\(\\)", features$Feature_name)] #select features met mean() of std()
  column_selection <- c(as.character(features_mean_std), "Subject", "Activity") #list of colum selection incl subject an activity
  
  Data_total <- subset(Data_total,select=column_selection) #select columns defined in feature selection from total data set
  
  
#3 Uses descriptive activity names to name the activities in the data set
  
  Data_total$Activity <- as.character(Data_total$Activity) #From int to character / string
  
  #Change numbers to activity names
  Data_total$Activity[Data_total$Activity == 1] <- "Walking"
  Data_total$Activity[Data_total$Activity == 2] <- "Walking Upstairs"
  Data_total$Activity[Data_total$Activity == 3] <- "Walking Downstairs"
  Data_total$Activity[Data_total$Activity == 4] <- "Sitting"
  Data_total$Activity[Data_total$Activity == 5] <- "Standing"
  Data_total$Activity[Data_total$Activity == 6] <- "Laying"
  
#4 Appropriately labels the data set with descriptive variable names.
  
  #Change Column names
  names(Data_total)<-gsub("mean\\(\\)", "Mean_", names(Data_total))
  names(Data_total)<-gsub("std\\(\\)", "StandardDeviation_", names(Data_total))
  names(Data_total)<-gsub("^t", "Time_", names(Data_total))
  names(Data_total)<-gsub("^f", "Frequency_", names(Data_total))
  names(Data_total)<-gsub("Jerk", "Jerk_", names(Data_total))
  names(Data_total)<-gsub("Acc", "Accelerometer_", names(Data_total))
  names(Data_total)<-gsub("Gyro", "Gyroscope_", names(Data_total))
  names(Data_total)<-gsub("Mag", "Magnitude_", names(Data_total))
  names(Data_total)<-gsub("BodyBody", "Body_", names(Data_total))
  names(Data_total)<-gsub("-", "", names(Data_total))
  
#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  Data_total <- data.table(Data_total) #Make table for aggregation
  
  TidyData <- Data_total[, lapply(.SD, mean), by = 'Subject,Activity']   #Mean of every column (.SD) by Subject and Activity

  write.table(TidyData, file = "TidyData.txt", row.names = FALSE)
  
