run_analysis <- function() {
      
      # Read all the files
      setwd("C:/zzx/R Programming/UCI HAR Dataset")
      library(reshape2)
      features <- read.table("features.txt")
      activity_labels <- read.table("activity_labels.txt")
      X_train <- read.table("train/X_train.txt")
      y_train <- read.table("train/y_train.txt")
      subject_train <- read.table("train/subject_train.txt")
      body_acc_x_train <- read.table("train/Inertial Signals/body_acc_x_train.txt")
      body_acc_y_train <- read.table("train/Inertial Signals/body_acc_y_train.txt")
      body_acc_z_train <- read.table("train/Inertial Signals/body_acc_z_train.txt")
      body_gyro_x_train <- read.table("train/Inertial Signals/body_gyro_x_train.txt")
      body_gyro_y_train <- read.table("train/Inertial Signals/body_gyro_y_train.txt")
      body_gyro_z_train <- read.table("train/Inertial Signals/body_gyro_z_train.txt")
      total_acc_x_train <- read.table("train/Inertial Signals/total_acc_x_train.txt")
      total_acc_y_train <- read.table("train/Inertial Signals/total_acc_y_train.txt")
      total_acc_z_train <- read.table("train/Inertial Signals/total_acc_z_train.txt")
      X_test <- read.table("test/X_test.txt")
      y_test <- read.table("test/y_test.txt")
      subject_test <- read.table("test/subject_test.txt")
      body_acc_x_test <- read.table("test/Inertial Signals/body_acc_x_test.txt")
      body_acc_y_test <- read.table("test/Inertial Signals/body_acc_y_test.txt")
      body_acc_z_test <- read.table("test/Inertial Signals/body_acc_z_test.txt")
      body_gyro_x_test <- read.table("test/Inertial Signals/body_gyro_x_test.txt")
      body_gyro_y_test <- read.table("test/Inertial Signals/body_gyro_y_test.txt")
      body_gyro_z_test <- read.table("test/Inertial Signals/body_gyro_z_test.txt")
      total_acc_y_test <- read.table("test/Inertial Signals/total_acc_y_test.txt")
      total_acc_z_test <- read.table("test/Inertial Signals/total_acc_z_test.txt")
      
      ## Naming variables according to description
      names(subject_train) <- "Subject"
      names(subject_test) <- "Subject"
      names(X_train) <- features$V2
      names(X_test) <- features$V2
      names(y_train) <- "Activity"
      names(y_test) <- "Activity"
      
      ## Merge train and test into one dataset
      train <- cbind(subject_train, y_train, X_train)
      test <- cbind(subject_test, y_test, X_test)
      total <- rbind(train, test)
      
      ## Change activities from integers to label names
      total$Activity <- factor(total$Activity, labels = activity_labels$V2)
      
      ## Extract columns with means or standard deviations
      meanstd <- grepl("mean\\(\\)", names(total)) | grepl("std", names(total))
      
      ## Show subject and activity as well
      meanstd[1:2] <- TRUE
      extracted <- total[,meanstd]
      
      ## Make the data tidy
      molten <- melt(extracted, id = c("Subject", "Activity"))
      finalset <- dcast(molten, Subject+Activity ~ variable, mean)
      
      ## Save the final set into a csv file
      write.csv(finalset, "finalset.csv", row.names = FALSE)
      
      
}