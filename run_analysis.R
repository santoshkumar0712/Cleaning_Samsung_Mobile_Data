## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(reshape2)
#load activity labels
activity<-read.table("./UCI HAR Dataset/activity_labels.txt")
#head(activity)
activity_label<-activity[,2]
#head(activity_label)
#load features
features<-read.table("./UCI HAR Dataset/features.txt")[,2]
#features

# Extract only the measurements on the mean and standard deviation for each measurement.
extract_feature<-grepl("mean|std",features)

# Load and process X_test & y_test data.
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#update X_test dataset column name with feature name
names(X_test) = features

#update data set to have only extracted feature variables
X_test<-(X_test[,extract_feature])

# Load activity labels
y_test[,2] = activity_label[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"

#nrow(X_test);nrow(y_test);nrow(subject_test)
test_data<-cbind(subject_test,y_test,X_test)


# Load and process X_train & y_train data.
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

#update X_train dataset column name with feature name
names(X_train) = features

#update data set to have only extracted feature variables
X_train = X_train[,extract_feature]

# Load activity data
y_train[,2] = activity_label[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"

# Bind data
train_data <- cbind(subject_train, y_train, X_train)

# Merge test and train data
data = rbind(test_data, train_data)

id_labels   = c("subject", "Activity_ID", "Activity_Label")
# get variable name
data_labels = setdiff(colnames(data), id_labels)
# melt data based on id and variable name
melt_data = melt(data, id = id_labels, measure.vars = data_labels)

# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "./tidy_data.txt",row.names = FALSE)

