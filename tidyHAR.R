# 
# Creates the first data set, X_consolidated.txt by combining the training & test data sets
#
#

library(plyr)
library(dplyr)

tidyHAR <- function () {
      
      # File names to avoid types
      
      dataDir<-"UCI HAR Dataset"
      activityLookupsFname <- paste(dataDir, "activity_labels.txt", sep = "/")
      featureLookupsFname <- paste(dataDir, "features.txt", sep="/")
      
      trainDir<- paste(dataDir, "train", sep="/")
      trainDatasetFname <- paste(trainDir, "X_train.txt", sep = "/")
      trainSubjectFname <- paste(trainDir, "subject_train.txt", sep = "/")
      trainActivityFname <- paste(trainDir, "y_train.txt", sep = "/")  # Misleading file name
      
      testDir<- paste(dataDir, "test", sep="/")
      testDatasetFname <- paste(testDir, "X_test.txt", sep = "/")
      testSubjectFname <- paste(testDir, "subject_test.txt", sep = "/")
      testActivityFname <- paste(testDir, "y_test.txt", sep = "/")  # Misleading file name
      
      outputDir<-"HARtidyDS"
      
      bothDatasetFname <- paste(outputDir, "X_consolidated.txt", sep="/")
      
      ############################################################################################
      # Lookups - Activity & Features - these are common to both the Training & Test Data Sets
      ############################################################################################
      
      # Lookups - Activity
      
      activityLookups<- read.table(file=activityLookupsFname, sep = "")
      names(activityLookups) <- c("activityCode", "activity")
      
      # Lookups - Metrics col names - NOTE: Some column names are duplicates! Need to be un-duped
      # Force character col classes since default factor class makes it hard to dedup later
      
      featureLookups <- read.table(file=featureLookupsFname, sep = "", colClasses = c(class(1L), class("a")))
      names(featureLookups) <- c("featureCode", "feature")
      
      #dedup non-unique features - append index number to name of column
      
      dups <- duplicated(featureLookups$feature) # logic values equal to size of feature
      # dups_index <- seq(dups)[dups]            # get index of values that at true only
      dups_index <- which(dups)                  # get index of dup values
      featureLookups$feature[dups_index] <- paste(featureLookups$feature[dups_index], 
                                                  as.character(dups_index), sep= ".")
      
      # Cleanup col Names slightly.  Avoid doing too much cleanup since UCI DataSet is well known.
      
      # featureLookups$feature<-gsub("\\(\\)", "", featureLookups$feature) # get rid of brackets in feature names
      
      ##############################################################################
      # Process each of the Training & Testing datasets separately
      # 
      # In hindsight,  this would have well worth abstracting into a common function
      # to avoid the way too many cut & paste errors I made.   
      ##############################################################################
      
      # Prepare Train - set col names
      
      trainDataset<- read.table(file=trainDatasetFname, sep="")
      names(trainDataset) <- featureLookups$feature
      
      # Enrich with other columns - Subject from the Subject_train.txt
      
      trainSubjects<- read.table(file=trainSubjectFname, sep="") 
      names(trainSubjects) <- c("subject")  
      
      trainActivities <- read.table(file=trainActivityFname, sep ="")
      names(trainActivities) <- "activityCode"
      
      trainDataset<- trainDataset %>% select(contains("-mean"), contains("-std")) %>% 
            mutate(activityCode=trainActivities$activityCode, subject=trainSubjects$subject, datasetType="Train") 
      trainDataset<- merge(x=trainDataset, y=activityLookups, by.x="activityCode", by.y="activityCode")
      
      # Prepare Test - set col names
      
      testDataset<- read.table(file=testDatasetFname, sep="")
      names(testDataset) <- featureLookups$feature
      
      # Prepare Test - enrich with additional columns
      
      testSubjects<- read.table(file=testSubjectFname, sep="")
      names(testSubjects) <- c("subject")  
      
      testActivities <- read.table(file=testActivityFname, sep ="")
      names(testActivities) <- "activityCode"
      
      
      testDataset<- testDataset %>% select(contains("-mean"), contains("-std")) %>%
            mutate(activityCode=testActivities$activityCode, subject=testSubjects$subject, datasetType="Test") 
 
      testDataset<- merge(x=testDataset, y=activityLookups, by.x="activityCode", by.y="activityCode")
      
      # merge the data sets together
      
      bothDataset <- rbind(trainDataset, testDataset)
      
      
      # create an index field with all the rows sequentially numbered for tidy data
      
      bothDataset<- bothDataset %>% mutate(index=seq_along(bothDataset$subject)) # use any column to generate index
      
      # rearrange col-names so it columns are ordered better.
      
      numColsList <- names(bothDataset)[sapply(bothDataset,class) == "numeric"]
      orderedColNames <- c("index", "subject", "activityCode", "activity", "datasetType", numColsList)
       
      
      write.table(bothDataset[, orderedColNames], file=bothDatasetFname, sep="\t", row.names = FALSE)
      
      invisible(bothDataset) # return for testing or for deriving other tidy data sets
      
      
      
}