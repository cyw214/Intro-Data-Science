# Name: Charles Wallace
# Date: 4/12/2017
# Assignment: HW9
# Class: Introduction to Data Science

# load rpart library
library(rpart)
# load E1071 library, install.packages("e1071") if not already installed
library(e1071)

# Obtain the data set (without a header), and seperate by comma 
cancerData <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", sep=",",header=FALSE) 

# Name the columns
names(cancerData) <- c("Sample_ID", "Clump_Thickness", "Cell_Size_Uniformity", "Cell_Shape_Uniformity", "Marginal_Adhesion", "Epithelial_Cell_Size", "Bare_Nucleoli", "Bland_Chromatin","Normal_Nucleoli","Mitoses", "Class")

# Factor the class attribute
cancerData$Class <- factor(cancerData$Class, levels=c(2,4), labels=c("benign","malignant"))

# Remove Id's
cancerData <- cancerData[,-1]

# shuffle the data to remove bias of any initial ordering in the data set
cancerData <- cancerData[sample(nrow(cancerData)), ]

# Seperating Data Set indicies into 10 folds with cut: setting breaks to 10 to indicate cut points
# and labels to false to return an integer vector of equal length to the nrows in cancerData
kFolds <- cut(seq(1,nrow(cancerData)), breaks = 10, labels = FALSE)

# creating results data frame to store the precision, recall, F measure, and accuracy of each iteration
resultsDF <- data.frame(precision=numeric(), recall=numeric(), fmeasure=numeric(), accuracy=numeric())

# function to compute precision, recall, f-measure, and accuracy from confusion matrix
retMetrics <- function(confusion){
	# precision = tp / (tp + fp)
	precision <- confusion[4]/(confusion[4] + confusion[2])
	# recall = tp / (tp + fn)
	recall <- confusion[4]/(confusion[4] + confusion[3])
	# f-measure = 2 x precision x recall / (precision + recall)
	fmeasure <- (2*precision*recall) / (precision+recall)
	# accuracy = (tp + tn) / (tp + fp + tn + fn)
	accuracy <- sum(diag(confusion))/sum(confusion)
	return(c(precision, recall, fmeasure, accuracy))
}

# Function to print the average of results from k-folds
printAvgResults <- function(resultDF){
	names(resultsDF) <- c("Precision", "Recall", "FMeasure", "Accuracy")
	cat(paste("For 10 Fold Cross Validation"),"\n")
	cat(paste("Average Precision:\t", mean(resultsDF$Precision), "\n",sep=" "))
	cat(paste("Average Recall:\t\t", mean(resultsDF$Recall), "\n",sep=" "))
	cat(paste("Average F-measure:\t", mean(resultsDF$FMeasure), "\n",sep=" "))
	cat(paste("Average Accuracy:\t", mean(resultsDF$Accuracy), "\n",sep=" "))
}

for(fold in 1:10){
	# collect array of indicies for test set from kFolds vector corresponding to current fold in the for iteration
	testSetIndicies <- which(kFolds == fold)
	# use every row not inluded in the test fold for training
	train <- cancerData[-testSetIndicies,]
	# use testSetIndicies for testing
	test <- cancerData[testSetIndicies,]
	# classify train data set with Naive Bayes
	nbClassifier <- naiveBayes(train[,1:9],train[,10])
	# predict values for test set, placing result in a dataframe to be read into a table
	testPredict <- data.frame(predict(nbClassifier, test[,1:9]), test[,10])
	# create names for the test Predict class
	names(testPredict) <- c("Predicted Class", "Actual Class")
	# use testPredict data frame to create a confusion matrix
	confusionMatrix <- table(testPredict)
	# return precision, recall, f-measure, and accuracy in vector from retMetrics function
	measure <- retMetrics(confusionMatrix)
	resultsDF <- rbind(resultsDF, measure)
}
# call printAvgResults to print the mean of each metric from dataframe
printAvgResults(resultDF)









