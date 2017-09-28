


##############################################################################
#        Read data: Downloading,unzipping and reading data
##############################################################################

# Downloading
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

# Reading trainings tables:
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# Reading testing tables:
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# Reading feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')

# Reading activity labels:
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')
activityLabels[,2] <- as.character(activityLabels[,2])

##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################

# Assigning column names:
colnames(x_train) <- features[,2] 
colnames(y_train) <-"activity"
colnames(subject_train) <- "subject"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activity"
colnames(subject_test) <- "subject"

colnames(activityLabels) <- c('subject','activity')

# Merging train data in one set:
mrg_train <- cbind(subject_train, y_train, x_train)
sum(is.na(mrg_train))

# Merging test data in one set:
mrg_test <- cbind(subject_test,y_test, x_test)
sum(is.na(mrg_test))

# Merging all data in one set "mergedata":
mergedata <- rbind(mrg_train, mrg_test)
sum(is.na(mergedata))

write.table(mergedata, "mergedata.txt", row.names = FALSE, quote = FALSE)
View(mergedata)

# cleanup enviroment. remove unused data
remove(x_train, y_train, subject_train, x_test, y_test, subject_test, mrg_train, mrg_test,fileUrl)

##############################################################################
# Step 2 - Extracting only the measurements on the mean and standard deviation 
#          for each measurement
##############################################################################
# Extract only the data on mean and standard deviation
columnsToKeep <- grepl("subject|activity|mean|std", colnames(mergedata))
mergedataMeanStd <- mergedata[, columnsToKeep]

View(mergedataMeanStd)

##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
############################################################################## 

mergedataMeanStd$activity <- factor(mergedataMeanStd$activity, levels = activityLabels[,1], labels = activityLabels[,2])
mergedataMeanStd$subject <- as.factor(mergedataMeanStd$subject)

dataMeanStd<-mergedataMeanStd

View(dataMeanStd)

##############################################################################
# step 4. Using descriptive activity names to name the activities in the data set
############################################################################## 

#searching for matches
featuresWanted <- grep(".*mean.*|.*std.*", features[,2]) 
featuresWanted.names <- features[featuresWanted,2]

#replacement
featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names) 
featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names = gsub("^t", "time", featuresWanted.names)
featuresWanted.names = gsub("^f", "frequency", featuresWanted.names)
featuresWanted.names = gsub("Acc", "Accelerometer", featuresWanted.names)
featuresWanted.names = gsub("Gyro", "Gyroscope", featuresWanted.names)
featuresWanted.names = gsub("Mag", "Magnitude", featuresWanted.names)
featuresWanted.names = gsub("BodyBody", "Body", featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)

colnames(dataMeanStd) <- c("subject", "activity", featuresWanted.names)

View(dataMeanStd)

write.table(dataMeanStd, "dataMeanStd.txt", row.names = FALSE, quote = FALSE)

##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

#5.1 Making second tidy data set

tidy <- aggregate(. ~subject + activity, dataMeanStd, mean)
tidy <- tidy[order(tidy$subject, tidy$activity),]
sum(is.na(tidy))

#5.2 Writing second tidy data set in txt file

write.table(tidy, "tidy.txt", row.name=FALSE,quote = FALSE)
View(tidy)


