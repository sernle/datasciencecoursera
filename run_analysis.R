
getMeasurementsColumns <- function(columnNames, partern) {
  sapply(columnNames, function(element_name) grepl(partern, element_name))
}

getData <- function(typeData, activityCode, featureNames) {
  #get main file
  filePath <- paste(typeData,"/subject_",typeData,".txt",sep="")
  subject <- read.table(file = filePath, col.names=c("subjects"))
  subject[,1] <- as.numeric(subject[,1])
  filePath <- paste(typeData,"/X_",typeData,".txt",sep="")
  features <- read.table(file = filePath, col.names= featureNames)  
  #get messurement for mean and std only
  #name of features will be filer out special characters so we need to get real names in dataframe
  colNames <- names(features)
  mean_std_features <- colNames[getMeasurementsColumns(colNames, "[mM]ean|[sS]td")]
  features <- features[, mean_std_features]
  
  #activities
  filePath <- paste(typeData,"/y_",typeData,".txt",sep="")
  activities <- read.table(file = filePath, , col.names=c("activities"))
  activities[,1] <- as.numeric(activities[,1])
  #convert activities column to text according to activity code
  activities[,1] <- sapply(activities[,1], function(x) activityCode[x])
  
  cbind(subject, activities, features)
  #nrow(body_gyro_z)
}




activityCode <- read.csv("activity_labels.txt", sep=" ", header = FALSE, stringsAsFactors = FALSE)[,2]
featureNames <- read.csv("features.txt", sep=" ", header = FALSE, stringsAsFactors = FALSE)[,2]
tidyData <- rbind(getData("test", activityCode, featureNames), getData("train", activityCode, featureNames))
#step 1:4
write.table(tidyData, "tidyData.csv", row.names=FALSE)
#step 5
library(reshape2)
meltTidyData <- melt(tidyData, id=c("subjects", "activities"))
library(plyr)
averageMovementBySubjectAndActivities <- ddply(meltTidyDat,.(subjects, activities), summarize, mean=mean(value))
write.table(averageMovementBySubjectAndActivities, "averageMovementBySubjectAndActivities.csv", row.names=FALSE)


#totalOrderData <- totalData[order(totalData$subjects),]
