runningExample <- function() {
	runningExampleDataSet <- generateDataSet();
	#runningExampleDataSet <- generateSyntheticDataSet(10, 2, 0.6)
	
	runningExampleDataMatrix <- as.matrix(runningExampleDataSet[,2:3]); 
	resultLabels <- skinnyDipClusteringFullSpace(runningExampleDataMatrix); 
	
	labels <- generateAccuracyLabels(resultLabels, runningExampleDataSet[,4]);
	gtcols = runningExampleDataSet[,4]
	print(gtcols)
	gtcols[gtcols == 0] <- 8

	classCols <- labels[,2]
	labels <- labels[,1]
	classCols[1] <- 8
	print(labels)
	print(classCols)
	resultLabels[resultLabels == 0] <- 8;

	plot(runningExampleDataMatrix[,1], runningExampleDataMatrix[,2], col=gtcols, xlim=c(0,1), ylim=c(0,1));
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

	maxlabels <- maximizeLabels(labels, ground_truth)
	oldlabels <- unique(maxlabels)
	#labels <- labels[,1]
	
	result <- matrix(0, max(labels)+1, 5)

	for(i in 0:max(labels)) {
		valids <- (ground_truth == oldlabels[oldlabels[,2]==i, 1])
		reported <- (labels == i)
		found <- (valids == TRUE & reported == TRUE)
		falseNeg <- (valids == TRUE & reported == FALSE)
		falsePos <- (valids == FALSE & reported == TRUE)
		print(sum(valids))
		print(sum(reported))

		reportedtotal <- sum(reported)
		validtotal <- sum(valids)
		cat("Correct classification rate: ",sum(found)/validtotal,'\n')
		cat("False negatives rate: ",sum(falseNeg)/validtotal, '\n')
		cat("False positives rate: ", sum(falsePos)/reportedtotal, '\n')
		cat("Total points: ", sum(valids), '\n\n')
		result[i+1,] <- c(sum(found)/validtotal, sum(falseNeg)/validtotal, sum(falsePos)/reportedtotal, validtotal, i)		
	}

	return(result)
}

maximizeLabels <- function(labels, ground_truth) {
	
	## find best ground_truth label for the unsupervised labels
	## assumes that the best match will be the one with the highest overlap for each class
	shuffled <- matrix(labels, length(labels), 2)
	print(max(ground_truth))
	for(i in 0:max(ground_truth)) {
		truth <- (ground_truth == i)
		maxlabel <- 0
		maxcount <- 0
		for (j in 0:max(ground_truth)) {
			reported <- (labels == j)
			count <- sum(truth == TRUE & reported == TRUE)
			if (count > maxcount) {
				maxlabel <- j
				maxcount <- count
			}
		}
		## index into the highest matched labels and change them to the ground_truth
		cat("Count for, ",i,": ",maxcount," - labeled ",  maxlabel,'\n')
		shuffled[labels == maxlabel,1] <- i
	}

	return(shuffled)
}



generateDataSet <- function(noiseFraction0to1=0.8){

    if(noiseFraction0to1==0.8){
        set.seed(122);
    } else{
        set.seed(195);
    }
    clusterSize <- 200;
    xVals <- c();
    yVals <- c();
    c1x <- rnorm(clusterSize, 0.9, 0.02); c1y <- rnorm(clusterSize, 0.2, 0.02); c2x <- rnorm(clusterSize, 0.8, 0.02); c2y <- rnorm(clusterSize, 0.2, 0.02); c3x <- runif(clusterSize, 0.2, 0.205); c3y <- runif(clusterSize, 0.1, 0.9); c4x <- rnorm(clusterSize, 0.7, 0.01); c4y <- rnorm(clusterSize, 0.9, 0.01); c5x <- runif(clusterSize, 0.44, 0.56); c5y <- runif(clusterSize, 0.44, 0.56); c6x <- rnorm(clusterSize, 0.8, 0.02); c6y <- rnorm(clusterSize, 0.3, 0.02);
    xVals <- c(xVals, c1x,c2x,c3x,c4x,c5x, c6x);
    yVals <- c(yVals, c1y,c2y,c3y,c4y,c5y,c6y);
    numNonNoisePoints <- 0#length(xVals);
    numNoisePoints <- round((numNonNoisePoints*noiseFraction0to1)/(1-noiseFraction0to1));
    xVals <- c(xVals, runif(numNoisePoints));
    yVals <- c(yVals, runif(numNoisePoints));
    dataMatrix <- matrix(c(xVals, yVals), length(xVals), 2);

    groundTruthLabels <- replicate(length(xVals),0)
    groundTruthLabels[sqrt(rowSums((dataMatrix-matrix(replicate(length(xVals), c(0.9,0.2)), length(xVals), 2, byrow=TRUE))^2))<0.04] <- 1
    groundTruthLabels[sqrt(rowSums((dataMatrix-matrix(replicate(length(xVals), c(0.8,0.2)), length(xVals), 2, byrow=TRUE))^2))<0.04] <- 2
    groundTruthLabels[xVals>0.2 & xVals<0.205 & yVals>0.1 & yVals<0.9] <- 3
    groundTruthLabels[sqrt(rowSums((dataMatrix-matrix(replicate(length(xVals), c(0.7,0.9)), length(xVals), 2, byrow=TRUE))^2))<0.02] <- 4
    groundTruthLabels[xVals>0.44 & xVals<0.56 & yVals>0.44 & yVals<0.56] <- 5
    groundTruthLabels[sqrt(rowSums((dataMatrix-matrix(replicate(length(xVals), c(0.8,0.3)), length(xVals), 2, byrow=TRUE))^2))<0.04] <- 6

    dataMatrix <- cbind(matrix(1:length(xVals),length(xVals),1), dataMatrix, matrix(groundTruthLabels, length(xVals), 1))
    ## random permute
    dataMatrix <- dataMatrix[sample(1:length(xVals), length(xVals)),]
    set.seed(NULL);
    return(dataMatrix);    
}


