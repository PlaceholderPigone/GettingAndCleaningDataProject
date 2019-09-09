## Getting and Cleaning Data Final Project
### Script Explanation
### By Phil Barker

This is a script that cleans up the data from the Human Activity Recognition Using Smartphones dataset.
That dataset is a set of accelerometar and posiional vector data taken by phones, with the a set of 30 subjects doing various activities.
It was then transformed and processed various treatments done to it, such as fast fourier transforms. For more information on the original dataset

The data is stored in a test and a training set, with 3 main tables: The subject number index table, the activity number table, and the observational data (or features) table,
which are labeled Subject, Y and X respectively. The Inertial Signals folder of the data is not used in this script

[For more information on the original data please check the original dataset found here](https://archive.ics.uci.edu/ml/datasets/human+activity+recognition+using+smartphones)

This particular script's purpose is to, if placed in the base data folder, process the data to output a tidy data set that contains the average of every mean and 
standard deviation observation grouped by each individual subject action (so for example, there is a Subject 1 Walking average, a subject 28 Laying average, etc).

The script functions, by both of the train and test sets, doing the following:
 * Reads the subject, X and Y tables
 * Appending the activity table(labeled Y) to the subject number table via cbind
 * Using a for loop to name the feature variables in the features table (labeled X) based on the features.txt file, which was processed with gsub into a clean vector
 * Using the same loop, selecting columns containing mean and standard deviation variables in the X data set and appending them to the above table via cbind. 
    * (Note, while it might be possible to do something that automatically searches for mean and standard deviation data columns, the presence of false positive columns such as freqmean and gravmean 
    led me to manually create the column index)
 * Merging both the train and test data sets
 * Naming the activities based on the index number and the activity_labels.txt file
 * Finally, using the summarize function to take the average of each file, grouped by subject number and then activity type and writing the table created
 
The script returns a txt file in the starting base directory of the data files.

