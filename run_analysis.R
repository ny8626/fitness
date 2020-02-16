#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy
#data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project.
#You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for
#performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you
#performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This
#repo explains how all of the scripts work and how they are connected.
#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies
#like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to
#from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full
#description is available at the site where the data was obtained:
#  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#You should create one R script called run_analysis.R that does the following.
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.


setwd("C:/Users/C10114/R")

#Install dplyr, tidyr and tidyselect packagege

library(dplyr)
library(tidyr)
library(tidyselect)


#Read training and test files

X_train <- data.frame()
X_test <- data.frame()

X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header = FALSE)

y_train <- data.frame()
y_test <- data.frame()

y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", header = FALSE, colClasses = "character")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", header = FALSE, colClasses = "character")

#Read feature index and corresponding feature name (features.txt) as data.frame 
#Find out feature index which correspond feature name including mean() or std() and store the index for later process.
#Please note there are multiple feature name in features.txt which has the same name, which disturb to use feature 
#name directly to select mean() and std() from the names.

feature <- data.frame()
feature_final <- data.frame()

feature <- read.table("./data/UCI HAR Dataset/features.txt", header = FALSE, stringsAsFactors = FALSE)
x <- c("mean()", "std()")
feature_final <- feature[grepl(paste(x, collapse = "|"), feature$V2),]
feature_final <- as.character(feature_final[,1])

#Read activity index and correponsing activity name (activity_lables.txt) as data.frame

activity <- data.frame()
activity <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE, stringsAsFactors = FALSE)
colnames(activity) <- c("Activity", "ActivityNm")
as.character(activity[,1])

#Assign the feature index as character to each coloumn of X_train and X_test

colnames(X_train) <- feature[,1]
colnames(X_test) <- feature[,1]

#To identify Who performed each activity, read subject_train and cbind the data.frame and X_train which was read above.

rowpersonnel_train <- data.frame()

rowpersonnel_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", header = FALSE, colClasses = "character" )
colnames(rowpersonnel_train) <- "Personnel"
X_train <- cbind(rowpersonnel_train,X_train)

colnames(y_train) <- "Activity"
X_train <- cbind(y_train,X_train)

#to identify Who performed each activity, read subject_test and cbind the data.frame and X_test which was read above.

rowpersonnel_test <- data.frame()

rowpersonnel_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", header = FALSE, colClasses = "character" )
colnames(rowpersonnel_test) <- "Personnel"
X_test <- cbind(rowpersonnel_test,X_test)

colnames(y_test) <- "Activity"
X_test <- cbind(y_test,X_test)

#1. Merges the training and the test sets to create one data set.

data <- data.frame()

data <- rbind(X_test,X_train)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.

final <- data.frame()

final <- select(data,Activity,Personnel,feature_final)
colnames(final) <- c("Activity","Personnel",feature[feature_final,2])
final <- merge(activity,final)
answer <- select(final,-Activity)

##
#From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.

library(reshape2)

final <- select(final,-Activity)

final_melt <- melt(final, id = c("ActivityNm","Personnel"), variable.name="Variable", na.rm=TRUE)
final_average <- dcast(final_melt,Personnel + ActivityNm ~ Variable,mean)

write.table(final_average,"C:/Users/C10114/R/Data/tidy_step5.txt",row.name=FALSE)
write.table(colnames(final_average),"C:/Users/C10114/R/Data/tidy_step5_feature_list.txt",row.name=FALSE)
                