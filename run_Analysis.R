source("tidyHAR.R")
source("createHARAvgDataset.R")

# This is just a driver script that relies on the workhorses - tidyHAR() and createHARAvgDataset() functions.
# Does not do much else than calling those 2 functions.  

run_Analysis<-function() {
      
      # create the tidy data set for HAR data
      
      print("Creating tidy data set for HAR observations - mean and std deviations...")
      harDF <- tidyHAR()
      
      # create the tidy data set for the avg of HAR data
      
      print("Creating tidy data set for HAR observations - mean grouped by subject and activity ...")
      createHARAvgDataset(harDF = harDF)
      
}