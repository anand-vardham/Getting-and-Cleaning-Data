run_analysis <- function(datasetFile) {
    # Path to the dataset zip file
    unzip(zipfile=datasetFile ,exdir="./data")

    if(!file.exists("./data")){
       dir.create("./data")
    }

    path_rf <- file.path("./data" , "UCI HAR Dataset")
    files <-list.files(path_rf, recursive=TRUE)
    
    # Load the Files
    activityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ), header = FALSE)
    activityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"), header = FALSE)

    subjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
    subjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)

    featuresTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
    featuresTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)

    # Merge the training and test data 
    subject <- rbind(subjectTrain, subjectTest)
    activity<- rbind(activityTrain, activityTest)
    features<- rbind(featuresTrain, featuresTest)

    # Uses descriptive activity names to name the activities in the data set
    names(subject)<-c("subject")
    names(activity)<- c("activity")
    featuresNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)
    names(features)<- featuresNames$V2

    # Merge the data
    combine <- cbind(subject, activity)
    data <- cbind(features, combine)

    # Extracts only the measurements on the mean and standard deviation for each measurement
    fillteredFeaturesNames<-featuresNames$V2[grep("mean\\(\\)|std\\(\\)", featuresNames$V2)]

    #Uses descriptive activity names to name the activities in the data set
    selectedNames <-c(as.character(fillteredFeaturesNames), "subject", "activity" )
    data <-subset(data,select=selectedNames)

    #Appropriately labels the data set with descriptive variable names.
    activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"),header = FALSE)
    names(data) <- gsub("^t", "time", names(data))
    names(data) <- gsub("^f", "frequency", names(data))
    names(data) <- gsub("Acc", "Accelerometer", names(data))
    names(data) <- gsub("Gyro", "Gyroscope", names(data))
    names(data) <- gsub("Mag", "Magnitude", names(data))
    names(data) <- gsub("BodyBody", "Body", names(data))
    str(data)

    #tidy data set with the average of each variable for each activity and each subject
    library(plyr);
    data2<-aggregate(. ~subject + activity, data, mean)
    data2<-data2[order(data2$subject,data2$activity),]
    write.table(data2, file = "tidydata.txt",row.name=FALSE)
    str(data2)
}


