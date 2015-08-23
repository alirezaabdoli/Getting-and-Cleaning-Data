run_analysis <- function()
{
      ## Initially load required libraries
      library(dplyr)
      library(reshape2)
      
      ## First navigate to the test subfolder of UCI HAR Dataset.
      original_dir <- getwd()
      setwd("UCI HAR Dataset/test")
      
      ## Read each of three .txt datasets within test folder
      ## and save to corresponding variables
      subject_test <- read.table(file = "subject_test.txt")
      X_test <- read.table(file = "X_test.txt")
      y_test <- read.table(file = "y_test.txt")
      
      ## Then navigate to the train subfolder of UCI HAR Dataset.
      setwd("../train")
      
      ## Next read each of three .txt datasets within train folder
      ## and save to corresponding variables
      subject_train <- read.table(file = "subject_train.txt")
      X_train <- read.table(file = "X_train.txt")
      y_train <- read.table(file = "y_train.txt")
      
      ## Now let's bind rows of (subject_test & subject_train),
      ## (y_test & y_train) and (X_test & X_train); the resulting
      ## datasets are put into "Subjects", "Activities", and
      ## "Measuremets" datasets.
      Subjects <- rbind(subject_test, subject_train)
      Activities <- rbind(y_test, y_train)
      Measurements <- rbind(X_test, X_train)
      
      ## Next we bind the three "Subjects", "Activities" and "Measuremets"
      ## datasets using cbind() function; the result is put into
      ## "df" dataset (containing 10299 rows and 563 columns)
      df <- cbind(Subjects, Activities, Measurements)
      
      ## Then we assign names to columns; so we initially need to read
      ## the txt file located at the "UCI HAR Dataset" folder; which is
      ## named "features.txt"; first we navigate back to the
      ## "UCI HAR Dataset" folder.
      setwd("../")
      Feats <- read.table("features.txt")
      
      ## As the "features.txt" contains duplicate values for some
      ## columns we need to fix this by using "unique" attribute
      ## of make.names() function.
      Feats <- make.names(Feats[, 2], unique = TRUE)
      
      ## Now we pass the appropriate values to the colnames()
      ## function in form of a character vector; index 1 being
      ## "Subjects", index 2 being "Activities", and indices
      ## 3 to 563 being the values stored in Feats variable.
      colnames(df)[1:563] <- c("Subjects", "Activities", Feats)
      
      ## According to the Course Project we only seek those columns
      ## containing "mean" and "std" values; thus we're going to use
      ## select() function from dplyr package such that the columns
      ## "Subjects", "Activities" and those columns containing either
      ## "mean" or "std" values are preserved and the rest of columns
      ## are omitted. This return a dataset of 10299 rows and 88 columns.
      df <- select(df, Subjects, Activities, contains("mean"), contains("std"))
      
      ## Next we attempt to appropriately name the values in the "Activities"
      ## column corresponding to activities performed by each subject (person)
      ## participating in the experiment; to fulfill this requirement we
      ## apply the acivity names provided in the "activity_labels.txt" file
      ## located at the "UCI HAR Dataset" folder
      Activity_Labels <- read.table("activity_labels.txt")
      
      ## Next we use the follwing command to utilize indexing for replacing
      ## numbers in the "Activities" column of "df" dataset with appropriate
      ## activity names stored in "Activity_Labels" variable.
      df[, 2] <- Activity_Labels[df[, 2], 2]
      
      ## get back to the original working directory
      setwd(original_dir)
      
      ## Next we write up the "df" dataset to a txt file named "tidy_df"
      write.table(df, "tidy_df.txt", row.names = FALSE)
      
      ## Finall we produce an imdependent dataset with the average of each 
      ## variable for each activity and each subject
      
      ## Thus we use melt() function from dplyr package and dcast from
      ## reshape2 package to get the average of each variable for each 
      ## activity and each subject; finally the new tidy dataset
      ## containing means of each variable for each activity and each 
      ## subject
      molten_df <- melt(df, id.vars = c("Subjects", "Activities"))
      mean_tidy_df <- dcast(molten_df, Subjects + variable ~ Activities, mean)
      write.table(mean_tidy_df, "mean_tidy_df.txt", row.names = FALSE)
}