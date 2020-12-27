## run_analysis.R

## Instructions:
## You should create one R script called run_analysis.R that does the following.

## 1.  Merges the training and the test sets to create one data set.
## 2.  Extracts only the measurements on the mean and standard deviation for each measurement.
## 3.  Uses descriptive activity names to name the activities in the data set
## 4.  Appropriately labels the data set with descriptive variable names.
## 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## Step 1: Identify what tables will want
## each variable should be in 1 column
## each observation of that variable should be in 1 row
## there should be 1 table for each "kind" of variable
## each table should include a column that allows them to be linked



## Experiment covered 6 activities.  
## 1 Walking
## 2 Walking_Upstairs
## 3 Walking_Downstairs
## 4 Sitting
## 5 Standing
## 6 Laying

## All subjects should have done all activities

## so each Column is an average of each variable provided in the data set
## and each Row is a subject

## There is an additional table which records information
## about generating this tidy data set.  It does not contain
## any data from the experiments, it is strictly recording
## steps made to get the tidy data.  The name of this table is
## TidyProcess

run_analysis <- function(datapath = "C:/Users/steph/data/", 
                         datafilename="analysis.zip",
                         review = FALSE){
    library(data.table)
    library(tidyverse) 
    library(tidyr)
    library(dplyr)
    library(stringi)
    library(reshape2)
    library(RCurl)
    library(zip)
    library(collapse)
   
   
    
      today <- Sys.Date()
      TidyProcess <- data.table(when = today, what = character)
      setwd(datapath)
      
      ## Get the raw data
 
      dataURI <- 
       "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

      ## if the file hasn't been downloaded go and get it
    
      if(!file.exists(paste(datapath,datafilename,sep="" ))){
          download_status <- download.file(dataURI, 
                          destfile = paste(datapath,datafilename,sep="" ),
                         method="curl")
          TidyProcess <- rbind.data.frame(
            Sys.Date(),paste(dataURI,as.character(download_status)))
          ## note that download_status ==0 is success
          ## Get the data from the zip file
          unzip(datafilename, exdir = datapath)
          TidyProcess <- rbind.data.frame(
          Sys.Date(),"unzip from zip library")
        } else {
            TidyProcess <- rbind.data.frame(
              Sys.Date(),"already on disk")
        } ## end if .zip file already on disk

      ## set the working directory
      setwd(datapath)

      setwd("UCI HAR Dataset") ## This works on Windows 10; it may not on your platform
    
      ## read in training set data
      xvals <- read.table("train/X_train.txt")
      yvals <- read.table("train/Y_train.txt")
      features <- read.table('features.txt')
      subject <- read.table("train/subject_train.txt")

      colnames(xvals) <- features[,2]

      activityfactor <- factor(c("walk",
                        "walkup",
                        "walkdown",
                        "sitting",
                        "standing",
                        "laying"))
    
      convertTofactor <- function (aNumber){
        return(activityfactor[aNumber])
      }
      
      colnames(yvals) <- "activityNumber"
    
      ## change activities from number to descriptive factor

      yvals <- yvals %>% mutate(activity = convertTofactor(activityNumber),
                              .keep = "none")

      trainingOrTesting <- factor(c("train","test"))
      xvals$subject <- subject[,1]
      xvals$activity <- yvals$activity
      ## every data point so far is from training
      xvals["datafrom"]="train"

      ## read in testing set data.  same process as training
      xtestvals <- read.table("test/X_test.txt")
      ytestvals <- read.table("test/Y_test.txt")
      testfeatures <- read.table('features.txt')
      testsubject <- read.table("test/subject_test.txt")
    
      colnames(xtestvals) <- features[,2]
      colnames(ytestvals) <- "activityNumber"
      ytestvals <- ytestvals %>% mutate(activity = convertTofactor(activityNumber),
                                          .keep = "none")
    
      xtestvals$subject <- testsubject[,1]
      xtestvals$test_activity <- ytestvals$testactivity
      ## want to make sure can distinguish 
      ## after merge between when activity was done in training or testing
      xtestvals["datafrom"]="test"

    
      ## join test and train
      trainAndtest <- full_join(xtestvals,xvals, by = "subject",
                                suffix = c(".test", ".train")) %>% ## remove rows that are all na
                      
                      subset(!is.na(subject) &
                               !is.na(activity) &
                               (!is.na(datafrom.train) | ## must have a subject and train or test
                               !is.na(datafrom.test)))
      ## full_join because want all rows in both training and testing data sets
      
      ## create a code book
      ## modifies and updates the available codebooks with the data to indicate all 
      ## the variables and summaries calculated, along with units, and any other relevant 
      ## information
      ## The code book checked into github is a .md (githup markdown) file
      ## this table here is just to help generate the data that is used in the actual
      ## codebook.md
      TrainandTestCodeBook <- data.table(columnTitle = colnames(trainAndtest))

      ## Actual CodeBook needs to have 3 columns, 1  = colnames and 2 = description 3= units/where
      ## number came from
      ## CodeBook written out as csv file so as to be easy to be editted by hand
      write.csv(TrainandTestCodeBook,"trainandtestcodebook.csv", 
                quote = FALSE)
      
    
      ## the following code was to test that tidy data was accomplished
      ## print(summary(trainAndtest))
      ## print(str(trainAndtest))
    
    
      ## Save the original data in the new format with training and testing merged
   
      saveRDS(trainAndtest,file="trainandtest.rda")
    
      ## Step 2:
      ## Extracts only the measurements on the mean and standard deviation 
      ## for each measurement
    
          ## columns which have mean have "mean" in them
          ## columns which have std deviation have "std" in them

    
      ## make a new table that just has the mean of each of the stdmeanCols
      ## with the new column name being mean_old column name

      justmean <- trainAndtest %>% 
        summarise(across(matches("*-mean*|*-std*"), fmean, .names = "mean_{.col}"))
      saveRDS(justmean,file="justmean.rda")
    
      ## does save and read work right on your operating system?
      ## note that load does not work on windows because object names must be the same
      ##-    really <- readRDS(file = "justmean.rda")
      ##-    if (!identical(justmean,really)){
      ##-        stop("readRDS of saveRDS not identical.  Data is corrupted.")
      ##-    } ## if not identical

    
    ## codebook for mean and std
    codebook <- c(
    "This file describes the data in justmean.rda   The format of this file is", 
    "Column Number. Column Name should you want to digitally process it.  This is the codebook", 
    "for Mean Generated Data.  Each row of this file has the column number followed by the column name. ",
    "If the column name ends in .test the mean is from the test data set.  If the column name end in ",
    ".train  the data is the mean from the train data set. The means are generated only on the mean and ", 
    "standard deviation columns of the test and train sets.  All other columns were ignored.  Information ", 
    "about the values of this dataset can be found in the features_info.txt file. The data in the ",
    "justmean.rda file is the mean, with NA values removed, of a column of data from the UCI HAR dataset " ,
    "that represents either a mean or a standard deviation.",
    "The name of the original column has been preserved after mean_",
    "The justmean.rda file has a header line which ",
    "names the columns with the following names:",
    
    "___",  ## write a horizontal line in the output
    "\r")
    theColnames <- colnames(justmean)
    for (i in 1:length(theColnames)){
      codebook <- c(codebook,paste(i,". ", theColnames[i], sep = ""))
    }
    write.table(codebook, file = "mscodebook", 
                col.names = FALSE, row.names = FALSE, 
                quote = FALSE)
 

    ## write out the TidyProcess data
    saveRDS(TidyProcess,file="tidyprocess.rda")
    
    ## make the files into a zip file
    zipname <- "run_analysis_results.zip"
    zip(
      zipfile = zipname,
      files = c("justmean.rda","tidyprocess.rda"), ## train and test is too big to upload to github
      include_directories = FALSE,
      root = ".",
      mode =  "cherry-pick"
    )

## To Review the tidy data please do the following
## setting file_uri to what is appropriate for your system
## I have set it here the name of the zip file I generated, but you will need to fix the path.

    if (review==TRUE){
      all_files <- zip_list(zipname)
 
                ## data frame with columns: 
                ## filename, ...

      unzip(zipname)
      ## check that save and then load files correctly preserved data
        for (i in 1:nrow(all_files)){
          print(all_files[i,"filename"])
          data <- readRDS(all_files[i,"filename"])
          View(data)
      } ## end for all the files
    }
}## end run_analysis.R