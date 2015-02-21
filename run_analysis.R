setwd("your working directory here")


##################################### STEP 1:  DATA ##########################################



################# load training data



### subject file:  read in, rename column as subject, and create id for each row as a numeric variable

train_subj <- read.table("./UCI HAR Dataset/train/subject_train.txt")
str(train_subj)
summary(train_subj)

library(dplyr)
train_subj <- rename(train_subj, subject = V1)

train_subj <- cbind(idTrain = rownames(train_subj),train_subj)
train_subj$idTrain <- as.numeric(as.character(train_subj$idTrain)) 



###  measurement data

# read in

train_X <- read.table("./UCI HAR Dataset/train/X_train.txt")
str(train_X)


# read in feature definitions and rename columns

features <- read.table("./UCI HAR Dataset/features.txt")

features <- rename(features, featureId = V1, featureName = V2)


# create vector of feature definitions then use with data.table function setnames
# to set these as variable names for the measurement data

featureNames <- as.vector(features$featureName)

library(data.table)
setnames(train_X, featureNames)


# create id for each row as a numeric variable

train_X <- cbind(idTrain = rownames(train_X),train_X)
train_X$idTrain <- as.numeric(as.character(train_X$idTrain)) 



### read in activities, rename column, and create id for each row as a numeric variable

train_Y <- read.table("./UCI HAR Dataset/train/Y_train.txt")
str(train_Y)

train_Y <- rename(train_Y, activityId = V1)

train_Y <- cbind(idTrain = rownames(train_Y),train_Y)
train_Y$idTrain <- as.numeric(as.character(train_Y$idTrain)) 



### merge subject, activity, and measurement dataframes by idTrain

train_final <- merge(train_subj, train_Y, by = "idTrain")

train_final <- merge(train_final, train_X, by = "idTrain")




################# load testing data



### subject file:  read in, rename column as subject, and create id for each row as a numeric variable

test_subj <- read.table("./UCI HAR Dataset/test/subject_test.txt")
str(test_subj)
summary(test_subj)

library(dplyr)
test_subj <- rename(test_subj, subject = V1)

test_subj <- cbind(idTest = rownames(test_subj),test_subj)
test_subj$idTest <- as.numeric(as.character(test_subj$idTest)) 



###  measurement data

# read in

test_X <- read.table("./UCI HAR Dataset/test/X_test.txt")
str(test_X)


# use vector of feature definitions (created above) with data.table function setnames
# to set these as variable names for the measurement data

setnames(test_X, featureNames)


# create id for each row as a numeric variable

test_X <- cbind(idTest = rownames(test_X),test_X)
test_X$idTest <- as.numeric(as.character(test_X$idTest))



### read in activities, rename column, and create id for each row as a numeric variable

test_Y <- read.table("./UCI HAR Dataset/test/Y_test.txt")
str(test_Y)

test_Y <- rename(test_Y, activityId = V1)

test_Y <- cbind(idTest = rownames(test_Y),test_Y)
test_Y$idTest <- as.numeric(as.character(test_Y$idTest)) 



### merge subject, activity, and measurement dataframes by idTrain

test_final <- merge(test_subj, test_Y, by = "idTest")

test_final <- merge(test_final, test_X, by = "idTest")




################# training and testing dataframes together



### append using bind_rows from dplyr

train_test <- bind_rows(train_final, test_final)


### reorder columns to bring idTest to second position

train_test <- train_test[,c(1,565,2:564)]


### id for all observations 

train_test <- cbind(idAll = rownames(train_test), train_test)
train_test$idAll <- as.numeric(as.character(train_test$idAll)) 

############################## STEP 2:  MEAN AND STD DEV FEATURES ONLY #######################################



### vector of all variables to keep

# create vector of features that are means or standard deviations from vector
# of feature names using grep (remove meanFreq with grepl)

incl <- grep("mean()|std()", features$featureName, value = TRUE)
incl <- incl[!grepl("Freq", incl)] # remove variables for meanFreq


# add first names of first five columns of combined train & test data

incl <- c(names(train_test[1:5]),incl)



### remove variables that are not means or standard deviations by selecting on 
# names in vector created above

train_test_msd <- subset(train_test, select = incl)



############################## STEP 3:  ACTIVITY LABELS #######################################


### read in activity labels and rename columns

labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
str(labels)

labels <- rename(labels, activityId = V1, activityName = V2)


### merge train_test_msd dataframe with activity labels by activityID

train_test_msd <- merge(train_test_msd, labels, by = "activityId")


### reorder columns 

train_test_msd <- train_test_msd[, c(2:5,1,72,6:71)]


### sort by idAll with arrange from dplyr package

train_test_msd <- arrange(train_test_msd, idAll)



######################################### STEP 4:  TIDY DATA ##########################################


### reduce dimensions of data- only need subject, activity label, and measurements

tidy_work <- subset(train_test_msd, select = -c(1,2,3,5))



### obtain mean for each of the 66 variables relating to feature means and std dev
# using ddply from plyr package.  specifying subject and activity as the 
# variables to group by, the numcolwise function will return the mean 
# for the remaining columns

library(plyr)

tidy_summ <- ddply(tidy_work, .(subject, activityName), numcolwise(mean))


### output to text file

write.table(tidy_summ, "tidy_summ.txt", sep = "\t", row.names = FALSE) 

