####install the packages and load libraries
library(readr)
library(dplyr)
library(tidyr)

####creating a directory
if(!file.exists("./d2")){dir.create("./d2")}

####Downloading file and Unzipped the dataset

url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile="./d2/Dataset.zip", method= "auto")
unzip(zipfile="./d2/Dataset.zip",exdir="./d2")

#getting list of files
files <-list.files((file.path="./d2/UCI HAR Dataset"), recursive=TRUE)
 
#Reading all data from Dataset
x_train <-read.table("./d2/UCI HAR Dataset/train/X_train.txt")
y_train <-read.table("./d2/UCI HAR Dataset/train/y_train.txt")
subject_train <-read.table("./d2/UCI HAR Dataset/train/subject_train.txt")
x_test <-read.table("./d2/UCI HAR Dataset/test/X_test.txt")
y_test <-read.table("./d2/UCI HAR Dataset/test/y_test.txt")
subject_test <-read.table("./d2/UCI HAR Dataset/test/subject_test.txt")
features <-read.table("./d2/UCI HAR Dataset/features.txt")
activityLabels <-read.table("./d2/UCI HAR Dataset/activity_labels.txt")


####Merging the training and test sets to create one dataset
mergeSubject<-rbind(subject_train, subject_test)
merge_activity<-rbind(y_train, y_test)
merge_features<-rbind(x_train, x_test)

#Creating column names for variables 
colnames(mergeSubject) <-"Subject_ID"
colnames(merge_activity)<- "Activity_ID"
colnames(merge_features) <- t(features[2])

##merge all data( "data" is the  complete merged one dataset)
merge_Dataset <- cbind(mergeSubject, merge_activity)
data<- cbind(merge_features, merge_Dataset)

####Extracts only the measurements on the mean and standard deviation for each measurement.("Data2" is the extracted data)
columnwithMean_Std <-grep(".*mean.*|.*std.*", names(data), ignore.case = TRUE)
totalcolumns <-c(columnwithMean_Std, 562,563)
Data2 <- data[, totalcolumns]

####Used descriptive activity names to name the activities in the data set
Data2$Activity_ID <-as.character(Data2$Activity_ID)
   for(i in 1:6){
     Data2$Activity_ID[Data2$Activity_ID == i]<- as.character(activityLabels[i,2])
   }

Data2$Activity_ID <- as.factor(Data2$Activity_ID)

####Appropriately labels the data set with descriptive variable names
names(Data2)<- gsub("^t", "Time", names(Data2))
names(Data2)<- gsub("^f", "Frequency", names(Data2))
names(Data2)<- gsub("Acc", "Accelerometer", names(Data2))
names(Data2)<- gsub("Gyro", "Gyroscope", names(Data2))
names(Data2)<- gsub("BodyBody", "Body", names(Data2))
names(Data2)<- gsub("Mag", "Magnitude", names(Data2))
names(Data2)<- gsub("tBody", "TimeBody", names(Data2))
names(Data2)<- gsub("angle", "Angle", names(Data2))
names(Data2)<- gsub("gravity", "Gravity", names(Data2))

####creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(data.table)
Data2$Subject_ID <- as.factor(Data2$Subject_ID)
Data2.dt<-data.table(Data2)

tidydata <- Data2.dt %>% group_by(Activity_ID, Subject_ID) %>% summarize_all(funs(mean))

####Writing a "Tidydata" dataset in a txt file. 
write.table(tidydata, file = "tidydata.txt", row.names = FALSE,col.names = TRUE)