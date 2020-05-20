library(dplyr)

features <- read.table(file.choose())
colnames(features) <- c("n","functions")

acitivitylabels <- read.table(file.choose())
colnames(acitivitylabels) = c("code","activity")

subject_test <- read.table(file.choose())
head(subject_test)
colnames(subject_test) <- c("subject")

x_test <- read.table(file.choose())
colnames(x_test) <- c(features$functions)

y_test <- read.table(file.choose())
colnames(y_test) <- c("code")
head(y_test)

subject_train <- read.table(file.choose())
head(subject_train)
colnames(subject_train) <- c("subject")

x_train <- read.table(file.choose())
colnames(x_train) <- c(features$functions)

y_train <- read.table(file.choose())
colnames(y_train) <- c("code")
head(y_train)


#Step 1: Merges the training and the test sets to create one data set.

mergex <- rbind(x_train,x_test)
mergey <- rbind(y_train,y_test)
subject <- rbind(subject_train,subject_test)
merged_data <- cbind(subject, mergey,mergex)
merged_data <- merged_data[, !duplicated(colnames(merged_data))]

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

tidydata <- merged_data %>% select(subject,code, contains("mean"), contains("std"))


#Step 3: Uses descriptive activity names to name the activities in the data set.
tidydata$code <- acitivitylabels[tidydata$code,2]
head(tidydata)

#Step 4: Appropriately labels the data set with descriptive variable names.

names(tidydata)[2] = "activity"
names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))
names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("angle", "Angle", names(tidydata))
names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))

names(tidydata)

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

FinalData <- tidydata %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

