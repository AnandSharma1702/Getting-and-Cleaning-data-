###Step - 1 Data import and data manipulation###
###Installing/calling required libraries###

> library(dplyr)

####read and view the required data sets -  train data & test data###
> subject_train <- read.table("C:/Users/343565/Desktop/Coursera/Getting and cleaning data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")
>   View(subject_train)

> X_train <- read.table("C:/Users/343565/Desktop/Coursera/Getting and cleaning data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
>   View(X_train)

> y_train <- read.table("C:/Users/343565/Desktop/Coursera/Getting and cleaning data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")
>   View(y_train)


> subject_test <- read.table("C:/Users/343565/Desktop/Coursera/Getting and cleaning data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")
>   View(subject_test)

> X_test <- read.table("C:/Users/343565/Desktop/Coursera/Getting and cleaning data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
>   View(X_test)

> y_test <- read.table("C:/Users/343565/Desktop/Coursera/Getting and cleaning data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="")
>   View(y_test)

### reading data description###

variable_names <- read.table("C:/Users/343565/Desktop/Coursera/Getting and cleaning data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")

### read activity labels###

> activity_labels <- read.table("C:/Users/343565/Desktop/Coursera/Getting and cleaning data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")

#### Data merging ( training and the test sets to create one data set) & Sanity check###
X_total <- rbind(X_train, X_test)

Y_total <- rbind(Y_train, Y_test)

Subject_total <- rbind(Subject_train, Subject_test)

###Checking data sanity (Verifying number of rows, columns after merge####

> summary(X_train)
> nrow(X_train)
> nrow(X_test)
> nrow(X_total)
> ncol(X_train)
> ncol(X_test)
> ncol(X_total)
> nrow(y_train)
> nrow(y_test)
> nrow(Y_total)
> nrow(subject_train)
> nrow(subject_train)
> nrow(Subject_total)


####Step -2 ####
###Extracting (only) the measurements (on the mean and standard deviation)###

selected_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]

X_total <- X_total[,selected_var[,1]]
\


####Step -3 ####
### Uses descriptive activity names to name the activities in the data set ###
colnames(Y_total) <- "activity"

Y_total$activitylabel <- factor(Y_total$activity, labels = as.character(activity_labels[,2]))

activitylabel <- Y_total[,-1]

####Step -4 ####
####Appropriately labels the data set with descriptive variable names####

colnames(X_total) <- variable_names[selected_var[,1],2]

###From the data set in step 4, creates a second, independent tidy data set with the average### # of each variable for each activity and each subject###

colnames(Sub_total) <- "subject"

total <- cbind(X_total, activitylabel, Sub_total)

total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))

write.table(total_mean, file = "C:/Users/343565/Desktop/Coursera/Getting-and-Cleaning-Data-Week-4-Assignment-master/tidydata.txt", row.names = FALSE, col.names = TRUE) 
