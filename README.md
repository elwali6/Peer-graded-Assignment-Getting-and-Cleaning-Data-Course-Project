Getting and Cleaning Data - Course Project
The purpose of this project is to demonstrate the ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

The created R script called run_analysis.R does the following:

1.	Download the dataset if it does not already exist in the working directory
2.	Load the activity and feature info
3.	Loads both the training and test datasets, keeping only those columns which reflect a mean or standard deviation
4.	Loads the activity and subject data for each dataset, and merges those columns with the dataset
5.	Merges the training and the test sets to create one data set
6.	Converts the activity and subject columns into factors
7.	Creates a tidy dataset that consists of the average (mean) value of each variable for each subject and activity pair.

The end result is shown in the file tidy.txt.

The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
