clusterLabels <- function(labels, data) {
	
	newLabels <- labels;
	#newLabels <- c(rep(0, length(labels)))
	newLabels[] <- 0;
	#print(newLabels)
	dimensions <- ncol(data);
	# Ignore 0 cluster, because we won't change rejection class
	for (class in 1:max(labels)) {
		mask = labels==class;
		evidence = data[mask,];
		
		if (dimensions < 2) { # TODO, update evidence in single dimension
			mean <- mean(evidence);
			variance <- (var(evidence));
			newLabels[labels==class] <- class;
		} else {
			print(dimensions);
			mean <- colMeans(evidence);
			variance <- (cov(evidence));
			scores = as.matrix(sqrt(mahalanobis(data, mean, variance)));
			scores = matrix(scores, nrow(data), 1)
			
			newPoints <- (scores<=3);

			evidence = data[newPoints,];

			#evidence = matrix(evidence, sum(scores[,1]), ncol(data));
			#plot(evidence[,1], evidence[,2], xlim=c(0,1), ylim=c(0,1));

			# udpate return labels
			#print(newPoints[,1])
			#print(newLabels[newPoints[,1]] == class)
			newLabels[newPoints] <- class;
		}
	}
	print(sum(newLabels != labels))
	#print(labels);
	#print(newLabels);
	return(newLabels)
}


clusterLabels2 <- function(labels, data) {

        newLabels <- labels;
        #newLabels <- c(rep(0, length(labels)))
        newLabels[] <- 0;
        #print(newLabels)
        dimensions <- ncol(data);
        # Ignore 0 cluster, because we won't change rejection class
        for (class in 1:max(labels)) {
                mask = labels==class;
                evidence = data[mask,];

                if (dimensions < 2) { # TODO, update evidence in single dimension
                        mean <- mean(evidence);
                        variance <- (var(evidence));
                        newLabels[labels==class] <- class;
                } else {
			mean <- colMeans(evidence);
			ind_cov <- matrix(rep(0, dimensions^2), dimensions, dimensions)
			for (d in 1:dimensions) {
                        	ind_cov[d,d] <- var(evidence[,d]);
			}
			
                       	scores = as.matrix(sqrt(mahalanobis(data, mean, ind_cov)));
                       	scores = matrix(scores, nrow(data), 1)

       	                newPoints <- (scores<=3);

       	                evidence = data[newPoints,];

               	        #evidence = matrix(evidence, sum(scores[,1]), ncol(data));
                       	#plot(evidence[,1], evidence[,2], xlim=c(0,1), ylim=c(0,1));

                       	# udpate return labels
                       	#print(newPoints[,1])
                       	#print(newLabels[newPoints[,1]] == class)
                       	newLabels[newPoints] <- class;
                }
        }
        print(sum(newLabels != labels))
        #print(labels);
        #print(newLabels);
        return(newLabels)
}


# generate False Positive and False Negative error rates for
# Gaussian clusters of varying dimension using additional clustering
testDimensionsNew <- function(maxDim) {
	results = c();
	
	# test data on each dimensionality from 1 up to maxDim
	for (i in 1:maxDim) {
		# Generate data
		data <- generateGaussian(i);
		colCount <- ncol(data);
		dataMatrix <- as.matrix(data[,2:(colCount-1)]);
                resultLabels <- skinnyDipClusteringFullSpace(dataMatrix);
 		
		# perform additional clustering
		newLabels <- clusterLabels(resultLabels, dataMatrix);

		gtlabels <- data[,colCount];

                accuracies <- calculateAccuracy(newLabels, gtlabels);
		print(accuracies);

                results <- c(results, i, accuracies[2,2], accuracies[2,3]);
        }
        results <- matrix(results, 3, maxDim);
        plot(results[1,], results[2,]);
        lines(results[1,], results[2,], type='b');
        title("Single Gaussian Cluster FPR over Dimensionality", xlab="Dimensions", ylab="False Negative Rate")

	return(results);
}



# generate False Positive and False Negative error rates for
# Gaussian clusters of varying dimension using simple skinny-dip
testDimensionsOld <- function(maxDim) {
	results = c();
	for (i in 1:maxDim) {
		data <- generateGaussian(i);
		colCount <- ncol(data)
		dataMatrix <- as.matrix(data[,2:(colCount-1)]);
		resultLabels <- skinnyDipClusteringFullSpace(dataMatrix);
		gtlabels <- data[,colCount];

		accuracies <- calculateAccuracy(resultLabels, gtlabels);
		print(accuracies[2,2])

		results <- c(results, i, accuracies[2,2], accuracies[2,3]);
	}
	results <- matrix(results, 3, maxDim);
	plot(results[1,], results[2,]);
	lines(results[1,], results[2,], type='b');
	title("Single Gaussian Cluster FPR over Dimensionality", xlab="Dimensions", ylab="False Positive Rate")
	
	#plot(results[1,], results[3,]);
	#lines(results[1,], results[3,]);

	return(results);
}


compareSynthetic <- function() {
	
}



runningExample <- function(dimensions=3) {
        #runningExampleDataSet <- generateDataSet();
	#runningExampleDataSet <- generateSyntheticDataSet(10, 2, 0.6)
        runningExampleDataSet <- generateGaussian(dimensions);	

        colCount <- ncol(runningExampleDataSet);

	runningExampleDataMatrix <- as.matrix(runningExampleDataSet[,2:(colCount-1)]); 
        resultLabels <- skinnyDipClusteringFullSpace(runningExampleDataMatrix); 
	newLabels <- clusterLabels(resultLabels, runningExampleDataMatrix);
	resultLabels <- newLabels;
	
	labels <- generateAccuracyLabels(resultLabels, runningExampleDataSet[,colCount]);
	
        
        gtcols = runningExampleDataSet[,colCount]
	#print(gtcols)
	gtcols[gtcols == 0] <- 8

	classCols <- labels[,2]
	labels <- labels[,1]
	classCols[1] <- 8
	#print(labels)
	#print(classCols)
	resultLabels[resultLabels == 0] <- 8;

        if (dimensions == 1) {
		# hard coded 1D plot
		plot(runningExampleDataMatrix[,1], rep(0.5, nrow(runningExampleDataMatrix)), col=gtcols, xlim=c(0,1));
		legend("bottomleft", labels, bg="white",  pch='o', col=classCols, cex=0.75)
        	title("Accuracies in SkinnyDip Running Example")
	}
	else if (dimensions == 2) {
		# hard coded 2D plot
		plot(runningExampleDataMatrix[,1], runningExampleDataMatrix[,2], col=gtcols, xlim=c(0,1), ylim=c(0,1));
		legend("bottomleft", labels, bg="white",  pch='o', col=classCols, cex=0.75)
		title("Accuracies in SkinnyDip Running Example")
	}	
	else if (dimensions == 3) {
		# hard coded 3D plot
        	scatterplot3d(runningExampleDataMatrix[,1], runningExampleDataMatrix[,2], runningExampleDataMatrix[,3], color=gtcols)
        	legend("topleft", labels, bg="white", pch='o', col=classCols, cex=0.75)
	}
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

                negtotal = sum(!valids)
                reportedtotal <- sum(reported)
                validtotal <- sum(valids)

                falseNegRate = sum(falseNeg)/validtotal
                falsePosRate = sum(falsePos)/negtotal

		reportedtotal <- sum(reported)
		validtotal <- sum(valids)
		#cat("Correct classification rate: ",sum(found)/validtotal,'\n')
		#cat("False negatives rate: ",falseNegRate, '\n')
		#cat("False positives rate: ", falsePosRate, '\n')
		#cat("Total points: ", sum(valids), '\n\n')
		result[i+1,] <- c(sum(found)/validtotal, falseNegRate, falsePosRate, validtotal, i)		
	}

	return(result)
}

maximizeLabels <- function(labels, ground_truth) {
	
	## find best ground_truth label for the unsupervised labels
	## assumes that the best match will be the one with the highest overlap for each class
	shuffled <- matrix(labels, length(labels), 2)
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
		#cat("Count for, ",i,": ",maxcount," - labeled ",  maxlabel,'\n')
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
    numNonNoisePoints <- length(xVals);
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


# Generate our own custom data in multiple dimension
generateMultiDim <- function(dims) {
    
}

generateGaussian <- function(dimensions = 2, noiseFraction0to1=0.7) {
    set.seed(195);
    pointsPerDimension <- 100;
    clusterSize <- pointsPerDimension*dimensions;
    nonNoiseCount <- clusterSize;
    noiseCount <- round((nonNoiseCount*noiseFraction0to1)/(1-noiseFraction0to1));

    dataMatrix <- c();
    
    for (i in 1:dimensions) {
        dataMatrix <- c(dataMatrix, rnorm(clusterSize, 0.5, 0.05), runif(noiseCount));    
    }
    
    # ground truth labels
    dataMatrix <- c(dataMatrix, rep(1, clusterSize), rep(0, noiseCount));


    # match data structure of paper data by adding leading column
    dataMatrix <- c(rep(-1, clusterSize), rep(-1, noiseCount), dataMatrix);

    # reshape 
    dataMatrix <- matrix(dataMatrix, (clusterSize+noiseCount), (dimensions+2));    
    # label underlying noise
    colCount = ncol(dataMatrix)
    dataMatrix[sqrt(rowSums((dataMatrix[,2:(colCount-1)]-matrix(rep(0.5, dimensions*(clusterSize+noiseCount)), clusterSize+noiseCount, dimensions))^2))<0.2,colCount] <- 1;

    # shuffle
    dataMatrix <- dataMatrix[sample(1:nrow(dataMatrix), nrow(dataMatrix)),]

    set.seed(NULL);
    return(dataMatrix);
    
}
