# Data Set Source -->  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

require(data.table)

# Functions performed by run_analysis.R:

# 1. Merging of training and test sets

train_X <- read.table("train/X_train.txt")
test_X <- read.table("test/X_test.txt")
X <- rbind(train_X, test_X)

train_Y <- read.table("train/y_train.txt")
test_Y <- read.table("test/y_test.txt")
Y <- rbind(train_Y, test_Y)

train_sub <- read.table("train/subject_train.txt")
test_sub <- read.table("test/subject_test.txt")
S <- rbind(train_sub, test_sub)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# 2.a Read features and set them as names of X data set
features <- read.table("features.txt", stringsAsFactors=FALSE)
feature_names <- features[, 2]
names(X) <- feature_names

# 2.b Find indices of mean and standard column
mean_std_indices <- grep("-mean\\(\\)|-std\\(\\)", feature_names)

# 2.c Extract only mean and standard deviation
X <- X[, mean_std_indices]


# 3. Uses descriptive activity names to name the activities in the data set.

# 3.a Read activity labels 
activities_labels <- read.table("activity_labels.txt", stringsAsFactors=FALSE)
names(activities_labels) <- c('id', 'value')
activities <- data.table(activities_labels, key="id")
names(Y) <- c('id')


# 3.b Merge activities and Y data tables to create data table with descriptive activity names
descriptive_activity_names <- merge(Y, activities, by="id", all.x=T, sort=F)
Y <- as.data.frame(descriptive_activity_names[,2])
names(Y) <- "activity"


# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
combined <- cbind(S, Y, X)
write.table(combined, "combined_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

# 5.a Find Unique Subjects and activities

uniq_Sub <- unique(S)[,1]
uniq_Act <- unique(activities_labels)[,1]
uniq_sub_length <- length(uniq_Sub)
uniq_Act_length <- length(uniq_Act)
total_variables <- dim(combined)[2]
result = combined[1:(uniq_sub_length*uniq_Act_length), ]
row = 1

# 5.b Create a result data set with mean of each variable for each activity and each subject
for (s in 1:uniq_sub_length) {
    for (a in 1:uniq_Act_length) {
        result[row, 1] = uniq_Sub[s]
        result[row, 2] = activities_labels[a, 2]
        tmp <- combined[combined$subject==s & combined$activity==activities_labels[a, 2], ]
        result[row, 3:total_variables] <- colMeans(tmp[, 3:total_variables])
        row = row+1
    }
}
write.table(result, "average_per_activity_per_subject.txt" , row.name=FALSE)