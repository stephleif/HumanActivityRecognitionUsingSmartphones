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

run_analysis <- function(datapath = "C:/Users/steph/data/", datafilename="analysis.zip"){
    library(data.table)
    library(tidyverse) 
    library(tidyr)
    library(dplyr)
    library(stringi)
    library(reshape2)
    library(RCurl)
    library(zip)
    library(collapse)
    
    
    isaMeanorStd <- function(aname){
      ## grep returns place pattern starts, so val > 0 means pattern was found
        ifelse(test = grepl(pattern = "*mean*|*std*", x = aname), 
               yes = return(TRUE),
               no = return(FALSE))
    }## end isaMeanorStd     

    
## Get the raw data
 
    dataURI <- 
       "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

    download_status <-1
    ##download_status <- download.file(dataURI, 
    ##                      destfile = paste(datapath,datafilename,sep="" ),
    ##                      method="curl")
    
    today <- Sys.Date()
    
    TidyProcess <- data.table(when = today, what = character)
    
    TidyProcess <- rbind.data.frame(
        Sys.Date(),paste(dataURI,as.character(download_status)))
    ## note that download_status ==0 is success
    
    ## Get the data from the zip file
    ## set the working directory
    setwd(datapath)
    ##unzip(datafilename, exdir = datapath)
    TidyProcess <- rbind.data.frame(
      Sys.Date(),"unzip from zip library")

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
    
    ## change activties from number to descriptive factor

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
    trainAndtest <- full_join(xtestvals,xvals, by = "subject")
    ## full_join because want all rows in both training and testing data sets
    
    ## the following code was to test that tidy data was accomplished
    ## print(summary(trainAndtest))
    ## print(str(trainAndtest))
    
    
    ## Save the original data in the new format with training and testing merged
   
    save(trainAndtest,file="trainandtest.rda")
    
    ## Step 2:
    ## Extracts only the measurements on the mean and standard deviation 
    ## for each measurement
    
    ## columns which have mean have "mean" in them
    ## columns which have std deviation have "std" in them

    
    ## make a new table that just has the mean of each of the stdmeanCols
    ## with the new column name being mean_old column name
    
    thisHasData <- function(theColumn){
      return(length(na.omit(theColumn) > 0))
    }
    
    myMean <- function(theData){
      return(mean(theData, na.rm = TRUE))
    }
    
    tralala <- trainAndtest %>% 
      summarise(across(matches("*mean*|*std*"), fmean, .names = "mean_{.col}"))
    View(tralala,"maybe")
    return(0)
}
    


## To Review the tidy data please do the following
## setting file_uri to what is appropriate for your system
## I have set it here to the github of the files I generated
##data <- read.table(file_uri, header = TRUE) 
##View(data)