
#downloads the compressed file to the working directory
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)

# uncompresses the archives in the working directory
unzip("getdata_projectfiles_UCI HAR Dataset.zip")

# builds six data frames from the following files: subject_test.txt; y_test.txt; X_test.txt; subject_train.txt; y_train.txt and X_train.txt.
my_datai <- read.table("./UCI HAR Dataset/test/subject_test.txt")
my_datay <- read.table("./UCI HAR Dataset/test/y_test.txt")
my_datax <- read.table("./UCI HAR Dataset/test/X_test.txt", sep = "")
my_dataitrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
my_dataytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
my_dataxtrain <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "")

# build an additional dataframe with the features.txt file
nombrevars <- read.table("./UCI HAR Dataset/features.txt") 

# name of the variables are assigned for each dataframe. 
# we took the decision to relate these DF with these names based on the number of and the description at www.
colnames(my_dataxtrain) <- nombrevars$V2
colnames(my_datax) <- nombrevars$V2
colnames(my_datay) <- "Activities"
colnames(my_dataytrain) <- "Activities"
colnames(my_datai) <- "Subject"
colnames(my_dataitrain) <- "Subject"

# for both groups, train and test, we build a dataframe where each observation is identified with an activity and a subject. in order to do so, we bind the columns
# we took the decision to relate these DF with these names based on the number of and the description at www.
test<-cbind(my_datai,my_datay,my_datax)
train<-cbind(my_dataitrain,my_dataytrain,my_dataxtrain)

# we remove the unbinded dataframes
rm(my_datai) 
rm(my_dataitrain)
rm(my_datax)
rm(my_dataxtrain)
rm(my_datay)
rm(my_dataytrain)

# We generate a variable to identify whether the subject belongs to the tested or train groups
#train$trainortest <-"traingroup"
#test$trainortest <-"testgroup"
# we build a unique dataframe where merging the test and train dataframes through the rbind command, adding the observation of one to the other´s. 
# this is possible as both dataframes share the same variables
traintest <- rbind(train, test)

# we remove the unbinded dataframes
rm(train)
rm(test)

# creates a new dataset with a subset of variables (those that contain ...) in order to Extracts only the measurements on the mean and standard deviation for each measurement
meanstd<-dplyr::select(traintest,contains("Subject") | contains("Activities") | contains("mean()") | contains("std()") ) # tuve que aclarar que era dplyr:: porque si no generaba masking con otro package
rm(traintest)

# Uses descriptive activity names to name the activities in the data set
meanstd <- mutate(meanstd, Activity = case_when(Activities == 1 ~ "walking", #condition 1
                                                Activities == 2 ~ "walkingupstairs", #condition 2
                                                Activities == 3 ~ "walkingdownstairs", #condition 3
                                                Activities == 4 ~ "sitting", #condition 4
                                                Activities == 5 ~ "standing", #condition 5
                                                Activities == 6 ~ "laying", #condition 6
))

meanstd <- dplyr::select(meanstd, -Activities) #elimino la columna que identifica activity con un número

#In order to appropriately label the data set with descriptive variable names, we perform the following changes:
# every word is in lower case
# eliminate "," "_" "-"
# when a "t" is at the beginning of a variable name, it is replaced by "time"
# when a "f" is at the beginning of a variable name, it is replaced by "freq"
# a typo ("bodybody") is corrected
names(meanstd) <-  tolower(names(meanstd)) 
names(meanstd) <- sub("-", "", names(meanstd)) 
names(meanstd) <- sub("_", "", names(meanstd))
names(meanstd) <- sub(",", "", names(meanstd))
names(meanstd) <- sub("-", "", names(meanstd))
names(meanstd) <- sub("^f", "freq", names(meanstd))
names(meanstd) <- sub("^t", "time", names(meanstd))
names(meanstd) <- sub("bodybody", "body", names(meanstd))

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydta <- group_by(meanstd, subject, activity) %>% 
  summarise_all(mean)
write.table(tidydta, file="tidydta.txt", row.name=FALSE)
data <- read.table("tidydta.txt", header = TRUE)