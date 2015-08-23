#[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
# Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector 
# Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

library(swirl)
library(dplyr)
library(plyr)

# function to download, unzip as needed
# please note that folder ./data will be created
download_data <- function() {
  
  # setup a folder for data download
  if(!file.exists("./data")) {
    dir.create("./data")
  }
  
  # download and extract zip if needed
  if(!file.exists("./data/data.zip")) {
    zipfile <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(zipfile, "./data/data.zip", mode="wb")
    unzip("./data/data.zip",overwrite = TRUE,exdir = "./data")
  }
  
  # carefull! 
  setwd("./data/UCI HAR Dataset/");
  
}

# setwd("/home/david/Dropbox/src/r/r-data-cleaning/data/UCI HAR Dataset/")

# current folder should be "UCI HAR Dataset/".
# If not please setwd accordingly.

# get feature relevant for the project: means's and std's
features <- read.csv("features.txt", sep=" ", header = FALSE)
relevant_features <- features[grepl("-mean\\(\\)",features$V2) | grepl("-std\\(\\)",features$V2),]

# get activity labels
activity_labels <- read.csv("activity_labels.txt", sep=" ", header = FALSE)
colnames(activity_labels) <- c("activity_code","activity_name")

# x
x_test <- read.fwf("./test/X_test.txt", widths=rep(16,561), colClasses="numeric") %>% select(relevant_features$V1)
test_subjects <- read.csv("./test/subject_test.txt", header = FALSE)
colnames(test_subjects) <- c("subject")

x_train <- read.fwf("./train/X_train.txt", widths=rep(16,561), colClasses="numeric") %>% select(relevant_features$V1)
train_subjects <- read.csv("./train/subject_train.txt", header = FALSE)
colnames(train_subjects) <- c("subject")

x <- rbind(
  cbind(test_subjects,x_test),
  cbind(train_subjects, x_train))

# y
y_test <- read.csv("./test/y_test.txt", header=FALSE, colClasses="numeric") 
y_train <- read.csv("./train/y_train.txt", header=FALSE, colClasses="numeric") 
y <- rbind(y_test, y_train)
colnames(y) <- c("activity_code")

# get activity_names
activity_names <- merge(x = y, y = activity_labels, by = "activity_code", all = TRUE)

data <- cbind(y, activity_names$activity_name, x)
colnames(data) <- union(c("activity_code","activity_label","subject"), relevant_features$V2)

final_data <- select(data, -activity_code)

set5 <- final_data %>% group_by(activity_label,subject) %>% summarise_each(funs(mean))

