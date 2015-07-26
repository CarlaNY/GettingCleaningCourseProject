# run_analysis.R
# Carla_NY
# Getting and Cleaning Data course project
# 20150726
#
# assignment:
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set 
#      with the average of each variable for each activity and each subject.
#
# The dataset includes the following files:
#        - 'README.txt'
#        - 'features_info.txt': Shows information about the variables used on the feature vector.
#        - 'features.txt': List of all features.
#        - 'activity_labels.txt': Links the class labels with their activity name.
#        - 'train/X_train.txt': Training set.
#        - 'train/y_train.txt': Training labels.
#        - 'test/X_test.txt': Test set.
#        - 'test/y_test.txt': Test labels.
#        The following files are available for the train and test data. Their descriptions are equivalent. 
#        - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
#        - 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
#        - 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
#        - 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 
#
# Library HMisc, plyr, reshape2, dplyr, stringr
# get the data 
# 1.Merges the training and the test sets to create one data set.

# use the features as column names
v_dframe_features <- read.table("./UCI HAR Dataset/features.txt")
v_features_formatted <- str_replace_all(unlist(v_dframe_features$V2),"[[:punct:]]",".")

# get the training
# ....  subjects dataset
v_dframe_training_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE,col.names=c("Subject"))
# ....  activity training set
v_dframe_training_activity_y <- read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE,col.names=c("ActivityID"))
v_dframe_training_subjact <- cbind(v_dframe_training_subjects,v_dframe_training_activity_y)
# ....  training data
v_dframe_training_data <- read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE,col.names=v_features_formatted)
v_dframe_training_all <- cbind(v_dframe_training_subjact,v_dframe_training_data)
# at this point v_dframe_training has all training data

# get the test data
# .... subjects dataset
v_dframe_test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE,col.names=c("Subject"))
# .... activity test dataset
v_dframe_test_activity_y <- read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE,col.names=c("ActivityID"))
v_dframe_test_subjact <- cbind(v_dframe_test_subjects,v_dframe_test_activity_y)
# .... test data
v_dframe_test_data <- read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE,col.names=v_features_formatted)
# .... combine
v_dframe_test_all <- cbind(v_dframe_test_subjact,v_dframe_test_data)

# combine into 1 monster df for all training and test data
v_dframe_all <- rbind(v_dframe_training_all, v_dframe_test_all)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# new column list of ones to extract -- 
#  if column = 'Subject'
#  if column = 'ActivityDesc'
# or if name of column contains mean or std
v_whichcolumns <- (grepl("Subject",names(v_dframe_all)) | grepl("ActivityID",names(v_dframe_all)) | grepl ("mean",names(v_dframe_all)) | grepl ("std",names(v_dframe_all)))
## using the new list of columns, splice down the data frame to just those columns
v_dframe_all <- v_dframe_all[,v_whichcolumns]
## for my testing... 
## v_hold_all <- v_dframe_all
##

# 3.Uses descriptive activity names to name the activities in the data set
v_dframe_activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE,col.names=c("ActivityID","ActivityDesc"))
# loop through, resetting the values in the v_dframe_all

for (i in 1:nrow(v_dframe_activity_labels)) {
        v_dframe_all[v_dframe_all$ActivityID==i,2] <- as.character(v_dframe_activity_labels$ActivityDesc[i])
}

# 4.Appropriately labels the data set with descriptive variable names.
# did this on the creation of the dfs by specifying the col.names parameter
# ... see above
#
# 5.From the data set in step 4, creates a second, independent tidy data set 
#      with the average of each variable for each activity and each subject.
v_dframe_melt_alldata <- melt(v_dframe_all,id=c("Subject","ActivityID"),measuser.vars=(grepl("mean",names(v_dframe_all)) | grepl("std",names(v_dframe_all))))
# summarize
v_dframe_summarized <- dcast(v_dframe_melt_alldata,Subject + ActivityID ~ variable,mean)

# output to a file
write.table(v_dframe_summarized,"CourseProject_TidyData.txt", row.names=FALSE)
