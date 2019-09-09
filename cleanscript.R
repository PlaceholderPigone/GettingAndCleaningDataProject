library(dplyr)
library(tidyr)
library(readr)

#read all sources into tables
tst <- read.table("test/subject_test.txt", header = FALSE)
xtst <- read.table("test/X_test.txt", header = FALSE)
ytst <- read.table("test/y_test.txt", header = FALSE)
trn <- read.table("train/subject_train.txt", header = FALSE)
xtrn <- read.table("train/X_train.txt", header = FALSE)
ytrn <- read.table("train/y_train.txt", header = FALSE)
featlist <- read.table("features.txt", header = FALSE)

#create index variables for later merging and assign to the subject ID frame
#The test index continues sequentially from the end of the train index
trnind <- c(1:nrow(trn))
tstind <- c((nrow(trn)+1):(nrow(trn)+nrow(tst)))
trn2 <- cbind (trnind, trn)
tst2 <- cbind(tstind, tst)

#Merge the subject and the activity list
mergedtst <- cbind(tst2, ytst)
mergedtrn <- cbind(trn2, ytrn)

#Index for all of the std & mean colums in the x set
xcolindex <- c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,
               227:228,240:241,253:254,266:271,345:350,424:429,
               503:504,516:517,529:530,542:543)
#make a vector of the features names with lowercase
#remove parantheses and hyphens from features names
featvec <- tolower(as.vector(featlist$V2))
featvec <- gsub("\\()", "", featvec) 
featvec <- gsub("-", "_", featvec) 
featvec <- gsub("^t", "timedomain_", featvec)
featvec <- gsub("^f", "frequencydomain_",featvec)
featvec <- gsub("bodybody", "body", featvec)
#Make a for loop with all feature columns in the index 
#to bind selected columns into the other data frame
for (i in xcolindex){
  colnames(xtst)[i] <- featvec[i]
  colnames(xtrn)[i] <- featvec[i]
  mergedtst <- cbind(mergedtst, xtst[i])
  mergedtrn <- cbind(mergedtrn, xtrn[i])
}

#Clean column names for binding both sets
colnames(mergedtst)[1:3] <- c("index", "subject_id", "activity")
colnames(mergedtrn)[1:3] <- c("index", "subject_id", "activity")

#Combine the Test and the Train sets
mergedfull <- rbind(mergedtrn, mergedtst)

#Replace activity values with appropriate activities
mergedfull$activity <- factor(mergedfull$activity, 
                              levels = c(1, 2, 3, 4, 5, 6),
                              labels = c("WALKING", "WALKING UPSTAIRS", "WALKING DOWNSTAIRS",
                                         "SITTING", "STANDING", "LAYING")
)

#Create Tidy Data file with averages for each data point
#Grouped by subject and activity type
tidysum <- mergedfull %>%
  group_by(subject_id, activity) %>%
  summarise_at(vars(timedomain_bodyacc_mean_x:frequencydomain_bodygyrojerkmag_std), mean, na.rm = TRUE)

#append average to the column names to designate the data transformation
for (i in 3:ncol(tidysum)){
  colnames(tidysum)[i] <- paste("average_", colnames(tidysum)[i], sep="")
}

#write the table
write.table(tidysum, file="PBassignment5tidytable.txt", row.name=FALSE)


