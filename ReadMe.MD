---
output: html_document
---

This ReadMe file is dedicated to the Course Project of Getting and Cleaning Data course. This file explains the contents of run_analysis.R function aimed at getting, and tidying an initial dataset in order to get the dataset prepared for further processing (in this case computing the average values of variables).

1. Initially the provided datasets are read into R using read.table() function.

2. Then corresponding datasets are row binded to each other using rbind() function ('subject_test and train_test), (y_test, y_train) and (X_test and X_train).

3. Then concatenate the three datasets using cbind() function to get a single dataset.

4. Next read the variable labels from "features.txt" file and assign them to the variables (columns) of the dataset.

5. Next keep the first two columns (Subjects and Activities) and by using select() function from dplyr packages extract those variables containing either "mean" or "sum" in their names.

6. Read the activity labels in "activity_labels.txt" and use the values to appropriately change the values in the "Activities" column (2nd column of dataset) with the appropriate activity names.

7. Now, we have the initial dataset tidy and ready, it's time to write the initial dataset to a file named "tidy_df.txt" using write.table() function.

8. The final task is to create a new dataset with the average of each variable for each activity and each subject; in order to fulfill this task we apply melt() function from dplyr package and dcast() function from reshape2 package.

9. Finally write the updated dataset to a new file named "mean_tidy_df.txt".

10. You must find two .txt files in the same directory as the run_analysis.R script with the names "tidy_df.txt" and "mean_tidy_df.txt".
