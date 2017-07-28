#!/usr/bin/Rscript

library(FSelector)
library(kknn)
library(ROCR)
library(caTools)

# function to get the raw train data
getRawTrainData <- function() {
   rawTitanicTrainData <- read.table("http://www.cse.lehigh.edu/~brian/course/2017/datascience/TitanicTrain.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
   return(rawTitanicTrainData)
}

# function to get the reaw test data
getRawTestData <- function() {
	rawTitanicTestData <- read.table("http://www.cse.lehigh.edu/~brian/course/2017/datascience/TitanicTest.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
	return(rawTitanicTestData)
}

# function to clean data
cleanTitanicData <- function(rawData){
	dataNames <- c("pclass","fare", "sex", "age","embarked","survived")
	data <- rawData[dataNames]
	data <- na.omit(data)
	data$fare <- round(data$fare, 2)
	data$age <- floor(data$age)
	return(data)
}

# Function to calculate information gain
retIG <- function(data){
	gain <- information.gain(survived~., data)
	return(gain)
}

# functio to build model for kknn
runKKNN <-function(trainData,testData, dist, kparam){
	kknn <- kknn(survived~.,trainData,testData,distance=dist, k=kparam)
	kknnfit <-  fitted(kknn)
	#kknnprediction <- prediction(as.numeric(kknnfit), as.numeric(testData$survived))
	return(kknnfit)
}

# function to build model for logistic regression
runGLM <- function(trainData, testData){
	glm <- glm(survived~.,data=trainData, family=binomial)
	glmFit <- predict(glm, testData, type='response')
	return(glmFit)
}

# function to create confusion matrix
retConfusion <- function(testData, prediction){
	confusion <- table(testData$survived, prediction > 0.5)
	return(confusion)
} 

# uses confusion matrix to calculate precision, recall, f-measure, and accuracy
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

# function to test parameters for kknn for specified range of distance and k parameters returns a dataframe of results
kknnResults <- function(trainData, testData, dstart, dend, kstart, kend){
	resultsDF <- data.frame(precision=numeric(), recall=numeric(), fmeasure=numeric(), accuracy=numeric(), distance=numeric() ,k=numeric(),stringsAsFactors = FALSE)
	for (dist in dstart:dend){
		for(k in kstart:kend){
			fit <- runKKNN(trainData,testData, dist, k)
			confusion <- retConfusion(testData, fit)
			measure <- retMetrics(confusion)
			params <- c(as.numeric(dist), as.numeric(k))
			newRow <- c(measure, params)
			resultsDF <- rbind(resultsDF, newRow)
		}
	}
	resultsNames <- c("precision", "recall", "fmeasure", "accuracy", "distance","k")
	names(resultsDF) <- resultsNames
	return(resultsDF)
}

# function to print best results from kknnResults function's datafrme
bestResults <- function(resultDF){
	print("Best Precision:")
	print(resultDF[resultDF$precision == max(resultDF$precision),])
	print("Best Recall:")
	print(resultDF[resultDF$recall == max(resultDF$recall),])
	print("Best F-Measure:")
	print(resultDF[resultDF$fmeasure == max(resultDF$fmeasure),])
	print("Best Accuracy:")
	print(resultDF[resultDF$accuracy == max(resultDF$accuracy),])
}

# Accepts measure, confusion matrix, and auc then prints results
printResults <- function(measure,confusion,auc){
	write("\nPerformance Measures:","")
	write("---------------------\n","")
	# create data frame to store performance measures
	PRFA <- data.frame(precision=numeric(), recall=numeric(), fmeasure=numeric(), accuracy=numeric(),stringsAsFactors = FALSE)
	# bind the measures to the dataframe
	PRFA <- rbind(PRFA, measure)
	# create some names for the new data frame
	measureNames <- c("Precision", "Recall", "F-Measure", "Accuracy")
	# assign names
	names(PRFA) <- measureNames
	# print the measures
	print(PRFA)
	#write(PRFA,"")
	write("\nConfusion Matrix:", "")
	write("-----------------","")
	# print the confusion matrix
	print(confusion)
	# print the area under the curve
	write(c("\nAUC: ","----",auc), "")

}
set.seed(88)

# Get the Training data and Testing Data
rawTrain <- getRawTrainData()
rawTest <- getRawTestData()

# Clean the Data
trainClean <- cleanTitanicData(rawTrain)
testClean <- cleanTitanicData(rawTest)

# testing for best parameters to knn
# results <- kknnResults(trainClean, testClean,1,50,1,30)
# bestResults(results)

# Run KKNN
knnFit <- runKKNN(trainClean, testClean, 9, 13)
#knnFit <- runKKNN(trainClean, testClean, 1, 5)
# Run GLM
glmFit <- runGLM(trainClean,testClean)




# Take the result of fitting with the knn model and create confusion matrix from test data
confusionKKNN <- retConfusion(testClean, knnFit)
confusionGLM <- retConfusion(testClean,glmFit)

# Calculate precision, recall, f-measure, and accuracy, returned as a vector in that order
measureKKNN <- retMetrics(confusionKKNN)
measureGLM <- retMetrics(confusionGLM)

# Create ROC Curves for knn and logistic regression
knnPredict <- prediction(knnFit, testClean$survived)
glmPredict <- prediction(glmFit, testClean$survived)

knnPerformance <- performance(knnPredict, 'tpr', 'fpr')
glmPerformance <- performance(glmPredict, 'tpr', 'fpr')



# Plot ROC
plot(knnPerformance, main="KNN and Logistic Reg. ROC Curves", col=2)
plot(glmPerformance, add = TRUE,col=3, text.adj = c(-0.2,1.7))
legend(0.6,0.2, c('KNN','Log. Reg.'), 2:3)
# save plot
dev.copy(png, 'compare.png')
dev.off()

# Calculate AUC for knn and logistic regression
knnauc <- performance(knnPredict, measure = "auc")@y.values[[1]]
glmauc <- performance(glmPredict, measure = "auc")@y.values[[1]]

# Display information gain 
rawig <- retIG(rawTrain)
cleanig <- retIG(trainClean)

write("####################################","")
write("#         Information Gain         #","")
write("####################################","")
write("\nInformation Gain For Raw Data Set:","")
write("----------------------------------","")
print(rawig)
write("\nInformation Gain For Clean Data Set:","")
write("-------------------------------------","")
print(cleanig)


# Display Results For KNN
write("\n####################################","")
write("# Results From K Nearest Neighbors #","")
write("####################################","")
printResults(measureKKNN,confusionKKNN,knnauc)

# Display Results from Loistic Regression
write("\n####################################","")
write("# Results From Logistic Regression #", "")
write("####################################","")
printResults(measureGLM,confusionGLM,glmauc)

