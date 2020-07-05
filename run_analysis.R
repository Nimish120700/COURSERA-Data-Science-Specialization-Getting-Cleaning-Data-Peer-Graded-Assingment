#STEP:1 Loading required libraries for the purpose of analysis
library(data.table)
library(dplyr)

#STEP:2 Downloading and Unzipping the dataset
filename <- "data.zip"
if(!file.exists(filename)){
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, filename, method="curl")
}
if(!file.exists("UCI HAR Dataset")){
  unzip(filename)
}

#STEP:3 Reading the necessary meta-data into R.
feature_names <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#STEP:4 Formatting the Train and Test text files.
  #Step4.1: Reading Test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE)
features_test <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE)
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE)
  #Step4.2 : Reading Train data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE)
features_train <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE)
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE)

#************************* PART 1 : Merge the training and the test sets to create one data set *********************************
#STEP:5 Merging the train and test datasets
subject_merged <- rbind(subject_train, subject_test)
features_merged <- rbind(features_train, features_test)
activity_merged <- rbind(activity_train, activity_test)

#STEP:6 Naming the Columns
#Here the transverse function is employed for naming step.
colnames(features_merged) <- t(feature_names[2])
colnames(subject_merged) <- "Subject"
colnames(activity_merged) <- "Activity"

#STEP:7 Merging all the data into one dataset
complete_dataset <- cbind(features_merged, activity_merged, subject_merged)

#***************************** PART 2 : Extracts only the measurements on the mean and standard deviation for each measurement ******************************
#STEP:8 Finding the columns where words 'Mean' and 'STD' appear using grep function
columns_with_mean_std <- grep(".*Mean.*|.*Std.*", names(complete_dataset), ignore.case=TRUE)

needed_columns <- c(columns_with_mean_std, 562, 563)
extracted_dataset <- complete_dataset[,needed_columns]


#************************* PART 3 : Uses descriptive activity names to name the activities in the data set **********************************
extracted_dataset$Activity <- as.character(extracted_dataset$Activity)
for(i in 1:6){
  extracted_dataset[extracted_dataset==i] <- as.character(activity_labels[i,2])
}

extracted_dataset$Activity <- as.factor(extracted_dataset$Activity)
#STEP:9 Giving appropriate labels 
names(extracted_dataset)<-gsub("Acc", "Accelerometer", names(extracted_dataset))
names(extracted_dataset)<-gsub("Gyro", "Gyroscope", names(extracted_dataset))
names(extracted_dataset)<-gsub("BodyBody", "Body", names(extracted_dataset))
names(extracted_dataset)<-gsub("Mag", "Magnitude", names(extracted_dataset))
names(extracted_dataset)<-gsub("^t", "Time", names(extracted_dataset))
names(extracted_dataset)<-gsub("^f", "Frequency", names(extracted_dataset))
names(extracted_dataset)<-gsub("tBody", "TimeBody", names(extracted_dataset))
names(extracted_dataset)<-gsub("-mean()", "Mean", names(extracted_dataset), ignore.case = TRUE)
names(extracted_dataset)<-gsub("-std()", "STD", names(extracted_dataset), ignore.case = TRUE)
names(extracted_dataset)<-gsub("-freq()", "Frequency", names(extracted_dataset), ignore.case = TRUE)
names(extracted_dataset)<-gsub("angle", "Angle", names(extracted_dataset))
names(extracted_dataset)<-gsub("gravity", "Gravity", names(extracted_dataset))

#FINAL STEP : Creating a secondary tidy data set
extracted_dataset$Subject <- as.factor(extracted_dataset$Subject)
extracted_dataset <- data.table(extracted_dataset)

tidyData <- aggregate(. ~Subject + Activity, extracted_dataset, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)