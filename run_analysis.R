# 1. Merges the training and the test sets to create one data set.

t1 <- read.table("./train/X_train.txt")
t2 <- read.table("./test/X_test.txt")
joe <- rbind(t1, t2)

t1 <- read.table("./train/subject_train.txt")
t2 <- read.table("test/subject_test.txt")
isobo <- rbind(t1, t2)

t1 <- read.table("./train/y_train.txt")
t2 <- read.table("./test/y_test.txt")
chappy <- rbind(t1, t2)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
joe <- joe[, indices_of_good_features]
names(joe) <- features[indices_of_good_features, 2]
names(joe) <- gsub("\\(|\\)", "", names(joe))
names(joe) <- tolower(names(joe))

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
chappy[,1] = activities[chappy[,1], 2]
names(chappy) <- "activity"

# 4. Appropriately label the data set with descriptive activity names.

names(isobo) <- "subject"
cleaned <- cbind(isobo, chappy, joe)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(isobo)[,1]
numSubjects = length(unique(isobo)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt", row.name=FALSE)