# From the consolidated data set created in tidyHAR,  we create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# Input file into this process:  tidyHAR() output file or a data frame
# Output tidy data set created for the mean of each numeric measure grouped by subject & activity
#           UCI HAR Dataset/meanBySubjectActivity.txt

# This script uses data.tables unlike the tidyHAR() - mostly to get experience in both approaches.
# 
# From a methodology perpective, 
#     1) computing the averages is a fairly straightforward use of the data table grouping mechanisms.   
#     2) Given the large number of numeric columns involved,  I used lapply on the SD data set to compute the means
#        across the nearly 79 numeric columns
#     3) The columns are printed with a unique index for each observation,  the subject, activity code and activity
#        followed by the 79 numeric columns for readability (probably best viewed in Excel)
#     
#  This function is callable either from run_Analysis or also in the command line passing in the 1st tidy data set, 
#  the X_consolidated.txt file.   This allowed easier testing & debugging


library(data.table)

createHARAvgDataset <- function(harDF=NULL,  harFname=NULL) {
      
      # Output directory and filename specification
      
      outputDir<-"HARtidyDS"
      
      avgHARFname <- paste(outputDir, "meanBySubjectActivity.txt", sep = "/")
      
      # validate the input parameters - we need either a X_Consolidated.txt path or a data-frame or data.table
      # passed in.  Otherwise flag an error
      
      if (is.null(harDF) & is.null(harFname)) {
            stop("harDF and harFname cannot both be unspecified")
      }
      
      if (!is.null(harFname)) {
            myHARdt <- fread(harFname, header=TRUE)
      }
      else if (class(harDF) == "data.frame") {
            myHARdt <- data.table(harDF)
      }
      else {
            myHARdt <- harDF
      }

      
      # We want to get means only for the numeric columns - there are also too many of them to list explicitly
      # build a list of columns that are numeric, to pass to SDcols
      
      sdColClasses <- sapply(myHARdt, class)
      sd.cols <- names(myHARdt)[sdColClasses == "numeric"]

      # add a monotonically increasing unique index column as well to the data table
      
      HARavgDataset <- myHARdt[, lapply(.SD, mean, na.rm=TRUE), 
                               by=list(subject, activityCode, activity),
                               .SDcols = sd.cols][, index:=seq_along(subject)]
      
      # We want a tidy output with the columns in the right order
      
      orderedColsList <- c("index", "subject", "activityCode", "activity", sd.cols)
      
      # Note:  The column list is a dynamic column list that needs the WITH=FALSE setting in data.table
      # Ref: http://stackoverflow.com/questions/28094645/select-subset-of-columns-in-data-table-r
      #
      # Note:  Since we are generating the index in code,  we are turning off the row.names setting to suppress the
      # default printing of line numbers by write.table - although these can act as unique indexes,  there is no column
      # name automatically printed.
      
      write.table(HARavgDataset[, orderedColsList, with=FALSE] ,file = avgHARFname, sep= "\t", row.names = FALSE)
      
      invisible(HARavgDataset) # return this in case we want to write yet more tidy data sets derived from this.
}