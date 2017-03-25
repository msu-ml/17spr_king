runningExample <- function() {
	runningExampleDataSet <- generateRunningExampleDataSet();
	runningExampleDataMatrix <- as.matrix(runningExampleDataSet[,2:3]); 
	resultLabels <- skinnyDipClusteringFullSpace(runningExampleDataMatrix); 
	#resultLabels <- maximizeLabels(resultLabels, runningExampleDataSet[,4])
	
	labels <- generateAccuracyLabels(resultLabels, runningExampleDataSet[,4]);

	classCols <- labels[,2]
	labels <- labels[,1]
	classCols[1] <- 8
	print(labels)
	print(classCols)
	resultLabels[resultLabels == 0] <- 8;

	plot(runningExampleDataMatrix[,1], runningExampleDataMatrix[,2], col=resultLabels);
	legend("bottomleft", labels, bg="white",  pch='o', col=classCols, cex=0.75)
	title("Accuracies in SkinnyDip Running Example")
	## plot(runningExampleDataMatrix[,1], runningExampleDataMatrix[,2], col=resultLabels);
}


generateAccuracyLabels <- function(labels, ground_truth) {
	accuracies <- calculateAccuracy(labels, ground_truth)
	accuracyLabels <- matrix("", nrow(accuracies), 2)

	for (i in 1:nrow(accuracies)) {
		accuracyLabels[i,] <- c(sprintf("Total: %d Correct: %.3f, FalseN: %f, FalseP: %f", accuracies[i,4], accuracies[i,1], accuracies[i,2], accuracies[i,3]), accuracies[i,5])
	}
	return(accuracyLabels)
}


calculateAccuracy <- function(labels, ground_truth) {

	labels <- maximizeLabels(labels, ground_truth)
	print(labels)
	oldlabels <- unique(labels[,2])
	print(oldlabels)
	labels <- labels[,1]
	print(labels)
	
	result <- matrix(0, max(ground_truth)+1, 5)

	for(i in 0:max(ground_truth)) {
		valids <- (ground_truth == i)
		reported <- (labels == i)
		found <- (valids == TRUE & reported == TRUE)
		falseNeg <- (valids == TRUE & reported == FALSE)
		falsePos <- (valids == FALSE & reported == TRUE)
		print(sum(valids))
		print(sum(reported))

		total <- sum(valids)
		cat("Correct classification rate: ",sum(found)/total,'\n')
		cat("False negatives rate: ",sum(falseNeg)/total, '\n')
		cat("False positives rate: ", sum(falsePos)/total, '\n')
		cat("Total points: ", sum(valids), '\n\n')
		result[i+1,] <- c(sum(found)/total, sum(falseNeg)/total, sum(falsePos)/total, total, oldlabels[i+1])		
	}

	return(result)
}

maximizeLabels <- function(labels, ground_truth) {
	
	## find best ground_truth label for the unsupervised labels
	## assumes that the best match will be the one with the highest overlap for each class
	shuffled <- matrix(labels, length(labels), 2)
	print(shuffled)
	for(i in 1:max(ground_truth)) {
		truth <- (ground_truth == i)
		maxlabel <- 0
		maxcount <- 0
		for (j in 1:max(ground_truth)) {
			reported <- (labels == j)
			count <- sum(truth == TRUE & reported == TRUE)
			if (count > maxcount) {
				maxlabel <- j
				maxcount <- count
			}
		}
		## index into the highest matched labels and change them to the ground_truth
		shuffled[labels == maxlabel,1] <- i
	}

	return(shuffled)
}
