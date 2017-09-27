



##############################################################################
#        Read data: Downloading and unzipping datase
##############################################################################

    #1.1 Downloading
     if(!file.exists("./data")){dir.create("./data")}
     fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
     download.file(fileUrl,destfile="./data/Dataset.zip")

    #1.2 Unzip dataSet to /data directory
    unzip(zipfile="./data/Dataset.zip",exdir="./data")


    
##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################

     #1.1 Reading files
         #.1.1 Reading trainings tables:
          x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
          y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
          subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
 
     #1.1.2 Reading testing tables:
          x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
          y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
          subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

     #1.1.3 Reading feature vector:
         features <- read.table('./data/UCI HAR Dataset/features.txt')

         #1.1.4 Reading activity labels:
         activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')
         activityLabels[,2] <- as.character(activityLabels[,2])

     #1.2 Assigning column names:
    
       colnames(x_train) <- features[,2] 
       colnames(y_train) <-"activityId"
       colnames(subject_train) <- "subjectId"

       colnames(x_test) <- features[,2] 
       colnames(y_test) <- "activityId"
       colnames(subject_test) <- "subjectId"

       colnames(activityLabels) <- c('activityId','activityType')

    #1.3 Merging all data in one set "mergedata":
    
       mrg_train <- cbind(y_train, subject_train, x_train)
       mrg_test <- cbind(y_test, subject_test, x_test)
       mergedata <- rbind(mrg_train, mrg_test)
       

##############################################################################
# Step 2 - Extracting only the measurements on the mean and standard deviation 
#          for each measurement
##############################################################################

    #2.1 Reading column names:
    
    colNames <- colnames(mergedata)
    
    #2.2 Create vector for defining ID, mean and standard deviation:
    
       # Extract only the data on mean and standard deviation
       #searching for matches
       featuresWanted <- grep(".*mean.*|.*std.*", features[,2]) 
       featuresWanted.names <- features[featuresWanted,2]
       #replacement
       featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names) 
       featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
       featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)
 
       
##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
############################################################################## 
             
    #3.3 Making nessesary subset from mergedata:
    
    colnames(mergedata) <- c("subject", "activity", featuresWanted.names)
    

    

##############################################################################
# step 4. Using descriptive activity names to name the activities in the data set
##############################################################################  

    #4.1 turn activities & subjects into factors
    mergedata$activity <- factor(mergedata$activity, levels = activityLabels[,1], labels = activityLabels[,2])
    mergedata$subject <- as.factor(mergedata$subject)
    
    write.table(mergedata, "tidy.txt", row.names = FALSE, quote = FALSE)
    View(mergedata)

    
##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################
      
    #5.1 Making second tidy data set

   secTidySet <- aggregate(. ~subject + activity, mergedata, mean)
   secTidySet <- secTidySet[order(secTidySet$subject, secTidySet$activity),]

   #5.2 Writing second tidy data set in txt file

   write.table(secTidySet, "secTidySet.txt", row.name=FALSE,quote = FALSE)
   View(secTidySet)
  
   