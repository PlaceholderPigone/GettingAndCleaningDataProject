library(dplyr)
library(tidyr)
library(readr)
setwd("c:/Users/Placeholder/Documents/Coursera2/CleanProj")

#read all sources into tables
tst <- read.table("test/subject_test.txt", header = FALSE)
xtst <- read.table("test/X_test.txt", header = FALSE)
ytst <- read.table("test/y_test.txt", header = FALSE)
trn <- read.table("train/subject_train.txt", header = FALSE)
xtrn <- read.table("train/X_train.txt", header = FALSE)
ytrn <- read.table("train/y_train.txt", header = FALSE)

#create index variables for later merging and assign to the subject ID frame
#The test index continues sequentially from the end of the train index
trnind <- c(1:nrow(trn))
tstind <- c((nrow(trn)+1):(nrow(trn)+nrow(tst)))
trn2 <- cbind (trnind, trn)
tst2 <- cbind(tstind, tst)

#Assign flags for whether a observation is test/train for tracking
tstb <- "TEST"
trnb <- "TRAIN"

#Since we only need the mean + standard deviation for the clean data set
#We can just extract those and rbind it to 
trnmeans <- rowMeans(xtrn)
trnsd <- apply(xtrn, 1, sd)
tstmeans <- rowMeans(xtst)
tstsd <- apply(xtst, 1, sd)

#Add Mean and Std
mergedtst <- cbind(tst2, ytst, tstmeans, tstsd, tstb)
mergedtrn <- cbind(trn2, ytrn, trnmeans, trnsd, trnb)

#Clean column names for binding both sets
colnames(mergedtst) <- c("Index", "SubjectID", "Activity", "MeasurementMean", "MeasurementStandardDeviation", "SetOrigin")
colnames(mergedtrn) <- c("Index", "SubjectID", "Activity", "MeasurementMean", "MeasurementStandardDeviation", "SetOrigin")

#Combine the Test and the Train sets
mergedfull <- rbind(mergedtrn, mergedtst)

#Replace activity values with appropriate activities
mergedfull$Activity <- factor(mergedfull$Activity, 
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c("WALKING", "WALKING UPSTAIRS", "WALKING DOWNSTAIRS",
                 "SITTING", "STANDING", "LAYING")
      )

tidysum <- mergedfull %>%
  group_by(SubjectID, Activity) %>%
  summarize(AverageMean = mean(MeasurementMean), 
            AverageStdDev = mean(MeasurementStandardDeviation))

  

