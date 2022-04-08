# Load the dplyr package
library(dplyr)
# creating a project zip file
project_file <- "project_data.zip"

# Downloading data into the project_file variable.
if (!file.exists(project_file)){
  url_data <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url_data, project_file, method="curl")
}  

# Unzipping the project file to UCI HAR Datset
if (!file.exists("UCI HAR Dataset")) { 
  unzip(project_file)
}

# Assigning variables to all data frames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# 1. Merges the training and the test sets to create one data set.
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)
merged_data <- cbind(subject_data, y_data, x_data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
Extract_m_s <- merged_data %>% select(subject, code, contains("mean"), contains("std"))

# 3. Uses descriptive activity names to name the activities in the data set.
Extract_m_s$code <- activities[Extract_m_s$code, 2]

# 4. Appropriately labels the data set with descriptive variable names.
names(Extract_m_s)[2] = "activity"
names(Extract_m_s)<-gsub("Acc", "Accelerometer", names(Extract_m_s))
names(Extract_m_s)<-gsub("Gyro", "Gyroscope", names(Extract_m_s))
names(Extract_m_s)<-gsub("BodyBody", "Body", names(Extract_m_s))
names(Extract_m_s)<-gsub("Mag", "Magnitude", names(Extract_m_s))
names(Extract_m_s)<-gsub("^t", "Time", names(Extract_m_s))
names(Extract_m_s)<-gsub("^f", "Frequency", names(Extract_m_s))
names(Extract_m_s)<-gsub("tBody", "Time_Body", names(Extract_m_s))
names(Extract_m_s)<-gsub("-mean()", "Mean", names(Extract_m_s), ignore.case = TRUE)
names(Extract_m_s)<-gsub("-std()", "Standars_Deviation", names(Extract_m_s), ignore.case = TRUE)
names(Extract_m_s)<-gsub("-freq()", "Frequency", names(Extract_m_s), ignore.case = TRUE)
names(Extract_m_s)<-gsub("angle", "Angle", names(Extract_m_s))
names(Extract_m_s)<-gsub("gravity", "Gravity", names(Extract_m_s))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Tidy_Data <- Extract_m_s %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(Tidy_Data, "Tidy_Data.txt", row.name=FALSE)

View(Tidy_Data)