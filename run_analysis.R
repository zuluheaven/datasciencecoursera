# Merges the training and the test sets to create one data set.
# files are saved in working directory/project
train<- read.table("UCI HAR Dataset/train/x_train.txt")
test<- read.table("UCI HAR Datas/test/X_test.txt")
combo <- rbind(train,test)
#Checkers
total <- nrow(combo) == nrow(test)+nrow(train)
c(nrow(train), nrow(test), nrow(combo), "merge_sum is:", total) #report_rows
#--
train<- read.table("UCI HAR Datas/train/y_train.txt")
test<- read.table("UCI HAR Datas/test/y_test.txt")
combo_y <- rbind(train,test)
#Checkers
total <- nrow(combo_y) == nrow(test)+nrow(train)
c(nrow(train), nrow(test), nrow(combo_y), "merge_sum is:", total) #report_rows
#--
train<- read.table("UCI HAR Datas/train/subject_train.txt")
test<- read.table("UCI HAR Datas/test/subject_test.txt")
combo_subject <- rbind(train,test)
#Checkers
total <- nrow(combo_subject) == nrow(test)+nrow(train)
c(nrow(train), nrow(test), nrow(combo_subject), "merge_sum is:", total) #report_rows
#final check
print("nrow report")
print(c("dataset =", nrow(combo), "activity=", nrow(combo_y), "subject= ", nrow(combo_subject)))

#--------
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Var     measures name (way data is found in the txt)
#location of data to add to, to read from: combo, features.txt
features <- read.table("project/features.txt")
ID <- grep("-mean\\(\\)|-std\\(\\)", features[,2], ignore.case = TRUE ) #identify items with means and SD, and id its number , use feature[,2] because the measurementslabels are in col2
#select the variables in Combo that matches the IDs
combo_selected <- combo [, ID]
#check if selection is correct (ID length shd = ncol SElected )
print(c("selected cols=", ncol(combo_selected), "# of ID:", length(ID)))
names(combo_selected) <-features[ID, 2]
head(combo_selected) # print out to check

#-------
#Uses descriptive activity names to name the activities in the data set
# b: rename all the numbers to activity lables in 
##location of data to add to, to read from: combo_y, activity_lablels.txt
lable <- read.table("project/activity_labels.txt")
#look at the colnames before working
colnames(lable)
colnames(combo_y)
# change names
combo_y[,1]= lable[combo_y[,1], 2]

#--
#appropriately labels the data set with descriptive variable names. 
names(combo_subject) <- "subject"
names(combo_y)<-"activity"
cleandt <- cbind(combo_subject, combo_y, combo_selected)
write.table(cleandt, "clean_selected_dt.txt")

#----
#2nd independent tidy data set with the average of each variable for each activity
#and each subject.

#although we already knw there are 30 subjects, we can code it just in case there are more like when we use other similiar data
subject = unique(cleandt$subject)
nsubject <-length(subject)
nsubject #printout value just to check
#use a for/if/whhile loop to calculate average for each subject
nacts = length(unique(cleandt$activity))
nacts#printout value just to check
numCols = dim(cleandt)[2] #we know there are 68, but we can write a code for autocheck
result = cleandt[1:(nSubjects*nActivities), ]


row = 1
for (s in 1:nsubject) {
  for (a in 1:nacts) {
    result[row, 1] = subject[s]
    result[row, 2] = lable[a,2]
    tmp <- cleandt[cleandt$subject==s & cleandt$activity==lable[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "clean_means.txt")
