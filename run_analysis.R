## Load in training data set
xtrain <- read.table("./train/X_train.txt")
ytrain <- read.table("./train/y_train.txt")
subjecttrain <- read.table("./train/subject_train.txt")
## For training data: Combine subject ID, training data measurements, and activity type into single data frame
traindata <- cbind(subjecttrain, xtrain, ytrain)
##Load in test data set
xtest <- read.table("./test/X_test.txt")
ytest <- read.table("./test/y_test.txt")
subjecttest <- read.table("./test/subject_test.txt")
## For test data: Combine subject ID, test data measurements, and activity type into single data frame
testdata <- cbind(subjecttest,xtest,ytest)
##combine training and test data
alldata <- rbind(traindata,testdata)
## Load in variable names
varnames <- read.table("./features.txt")
##Remove dash characters from variable names
varnames$V2 <- gsub("-","",varnames$V2)
##Remove parentheses from variable names
varnames$V2<- gsub("\\(","",varnames$V2)
varnames$V2<- gsub("\\)","",varnames$V2)
##Remove commas from variable names
varnames$V2<- gsub(",","",varnames$V2)
##Label merged data frames with cleaned variable names
colnames(alldata) <- c("Subject", varnames$V2, "Activity")
##Determine which columns to keep based on search for mean and std
meanl <- grepl("mean", colnames(alldata))
stdl <- grepl("std", colnames(alldata))
##Mark meanFreq columns for removal
meanFreql <- grepl("meanFreq", colnames(alldata))
##Subset data to include only mean and std columns
subsetteddata<- alldata[1:3,((meanl | stdl) & !meanFreql)]
##Bind Subject and Activity names to subsetted data
subsetteddata <- cbind(alldata[,c(1,563)],subsetteddata)
##Melt subsetted data frame with id variables of subject and activity
library(reshape2)
melted <- melt(subsetteddata, id=c("Subject","Activity"))
##Cast molten data frame to summarize all variables by mean for each Subject Activity pair
casted<- dcast(melted, Subject*Activity ~ variable,mean)
##Rename Activities
newlabels <- as.character(casted$Activity)
newlabels<- gsub("1","Walking",newlabels)
newlabels<- gsub("2","WalkingUpstairs",newlabels)
newlabels<- gsub("3","WalkingDownstairs",newlabels)
newlabels<- gsub("4","Sitting",newlabels)
newlabels<- gsub("5","Standing",newlabels)
newlabels<- gsub("6","Laying",newlabels)
##Convert activities back to factor
casted$Activity <- as.factor(newlabels)
##Write tidy data to text file
write.table(casted, file="tidy.txt",sep=',')
