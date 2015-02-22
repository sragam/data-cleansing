# The purpose of this project is to demonstrate ability to collect, work with, and clean a data set. 
# The goal is to prepare tidy data that can be used for later analysis. 
# The data is built from recordings of subjects performing daily activities while carrying smartphone

# The full description of the data set is available at the site where the data was obtained: 
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

# The R script called run_analysis.R does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject.


# Set the working directory where the data will be downloaded to
setwd("C:/Users/b02257a/Desktop/coursera/DataCleansing")

# Checks for data directory and creates one if it doesn't exist
if (!file.exists("data")) {
  message("Creating data directory")
  dir.create("data")
}
# if the sample data file does not exist already, download it
if (!file.exists("data/UCI HAR Dataset")) {
  # download the data
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  zipfile="data/UCI_HAR_data.zip"
  message("Downloading data")
  download.file(fileURL, destfile=zipfile)
  unzip(zipfile, exdir="data")
}

# 1. Merges the training and the test sets to create one data set.

# Load the data from the HAR data set


x_train <- read.table("data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
x_test <- read.table("data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
activity_labels <- read.table("data/UCI HAR Dataset/activity_labels.txt")
features <- read.table("data/UCI HAR Dataset/features.txt")

# Merge the data into a new, combined data frame
x.df <- rbind(x_test, x_train)
y.df <- rbind(y_test, y_train)
subject.df <- rbind(subject_test, subject_train)


# 4. Appropriately labels the data set with descriptive variable names. 
# Clean up the variable names from features, and apply them to x.df
# NB: this could also be done between steps (3) and (5) by limiting
# the cleaned data_labels to the columns selected in (2).
data_labels = sapply(features$V2, function(x) {gsub('[-(),]+','_', x)})
colnames(x.df) <- data_labels


# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_and_std_features <- grep('mean|std', features$V2, ignore.case=TRUE)
x.df <- x.df[,mean_and_std_features]

# 3. Uses descriptive activity names to name the activities in the data set

# Use the activity list in y.df combined with the names in activity_lables
# to update x.df with activity labels
for (i in 1:6) {
  x.df$activity[y.df$V1 == i] <- as.character(activity_labels[i,2])  
}


# 5. Creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject. 

# First, append the subjects to x.df
x.df$subject <- as.factor(subject.df$V1)

# Then, groupby activity and subject, aggregate by average (mean)
tidy <- aggregate(. ~ subject+activity, data=x.df, FUN=mean)

# And finally, export the dataset as a txt file
write.table(tidy, "tidy.txt", sep="\t")
