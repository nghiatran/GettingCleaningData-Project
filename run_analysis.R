library(plyr)

variables.ids <- NULL
variables.names <- NULL

getVariables = function(){
    variables = read.table('UCI HAR Dataset/features.txt')
    variables.ids <<- grep("mean\\(\\)|std\\(\\)",variables[,2],value=F)
    variables.names <<- variables[variables.ids,2]
}
getVariables()

##Merge training and test data sets
train.data <- read.table('UCI HAR Dataset/train/X_train.txt')
test.data <- read.table('UCI HAR Dataset/test/X_test.txt')
data <- rbind(train.data,test.data)
data <- data[,variables.ids]

##Clip measurements data with activity labels
train.activities = read.table('UCI HAR Dataset/train/y_train.txt',col.names ='activityId')
test.activities = read.table('UCI HAR Dataset/test/y_test.txt',col.names='activityId')
activities = rbind(train.activities,test.activities)
activities = join(activities,read.table('UCI HAR Dataset/activity_labels.txt',col.names = c('activityId','activityName')),by = 'activityId')
data <- cbind(data,activities[,'activityName'])

##Label the dataset with descriptive variable names
colnames(data) = variables.names
colnames(data)[length(colnames(data))] ='Activity'

##Add subject column
train.subjects = read.table('UCI HAR Dataset/train/subject_train.txt')
test.subjects = read.table('UCI HAR Dataset/test/subject_test.txt')
subjects = rbind(train.subjects,test.subjects)
data2 <- cbind(data,subjects)
colnames(data2)[length(colnames(data2))] ='Subject'

outputData <- aggregate(. ~ Subject + Activity, data2, mean,na.rm = T)

##write output data to a file
write.table(outputData,'tidy_data.txt',row.names=F)