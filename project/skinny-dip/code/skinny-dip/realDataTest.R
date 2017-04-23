runningDataTest <- function(new = TRUE, plotGT=FALSE) {
	runningExampleDataSet <- generateDataSet();
	colCount <- ncol(runningExampleDataSet);

        runningExampleDataMatrix <- as.matrix(runningExampleDataSet[,2:(colCount-1)]);
        resultLabels <- skinnyDipClusteringFullSpace(runningExampleDataMatrix);

	if (new == TRUE) {
        	resultLabels <- clusterLabels(resultLabels, runningExampleDataMatrix);
	}
        
	labels <- generateAccuracyLabels(resultLabels, runningExampleDataSet[,colCount]);

	
        resultLabels[resultLabels == 0] <- 8

        classCols <- labels[,2]
        labels <- labels[,1]
        classCols[1] <- 8
        #print(labels)
        #print(classCols)

        if (plotGT) {
                resultLabels <- runningExampleDataSet[,colCount];
        }

        resultLabels[resultLabels == 0] <- 8;

	#temp <- resultLabels;
	#resultLabels[temp==1] <- 6;
	#resultLabels[temp==2] <- 4;
	#resultLabels[temp==3] <- 1;
	#resultLabels[temp==4] <- 3;
	#resultLabels[temp==5] <- 2;
	#resultLabels[temp==6] <- 5;
	

        # hard coded 2D plot
        plot(runningExampleDataMatrix[,1], runningExampleDataMatrix[,2], col=resultLabels, xlim=c(0,1), ylim=c(0,1));
        legend("bottomleft", labels, bg="white",  pch='o', col=classCols, cex=0.75)
        title("SkinnyDip Running Example Ground Truth")

}


realDataTest <- function(filename, new = TRUE, plotGT=FALSE, dimensions=2) {
	path <- '../../experiments/data/real/'
	runningExampleDataSet <- read.table(paste(path,filename,sep=""), sep=",", header=FALSE)

	print(runningExampleDataSet)        

	rowCount <- nrow(runningExampleDataSet);
	colCount <- ncol(runningExampleDataSet);

	nameLabels <- unique(runningExampleDataSet[,colCount]);
	runningExampleDataSet[,colCount] <- as.numeric(runningExampleDataSet[,colCount])

	print(runningExampleDataSet)

        runningExampleDataMatrix <- as.matrix(runningExampleDataSet[,2:(colCount-1)]);
        resultLabels <- skinnyDipClusteringFullSpace(runningExampleDataMatrix);

        if (new == TRUE) {
                resultLabels <- clusterLabels(resultLabels, runningExampleDataMatrix);
        }
        
        labels <- generateAccuracyLabels(resultLabels, runningExampleDataSet[,colCount]);

        
        resultLabels[resultLabels == 0] <- 8

        classCols <- labels[,2]
        labels <- labels[,1]
        classCols[1] <- 8
        
	if (plotGT) {
		resultLabels <- runningExampleDataSet[,colCount];
	}
	resultLabels[resultLabels == 0] <- 8;

        if (dimensions == 1) {
                # hard coded 1D plot
                plot(runningExampleDataMatrix[,1], rep(0.5, nrow(runningExampleDataMatrix)), col=resultLabels, xlim=c(0,1));
                legend("bottomleft", labels, bg="white",  pch='o', col=classCols, cex=0.75)
                title("Accuracies in SkinnyDip Running Example")
        }
        else if (dimensions == 2) {
                # hard coded 2D plot
                plot(runningExampleDataMatrix[,1], runningExampleDataMatrix[,2], col=resultLabels, xlim=c(0,1), ylim=c(0,1));
                legend("bottomleft", labels, bg="white",  pch='o', col=classCols, cex=0.75)
                title("Accuracies in SkinnyDip Running Example")
        }
        else if (dimensions == 3) {
                # hard coded 3D plot
                scatterplot3d(runningExampleDataMatrix[,1], runningExampleDataMatrix[,2], runningExampleDataMatrix[,3], color=resultLabels)
                legend("topleft", labels, bg="white", pch='o', col=classCols, cex=0.75)
        }
	
}

roadDataTest <- function(new = TRUE, plotGT=FALSE) {
	filename <- '3D_spatial_network.txt'
        path <- '../../experiments/data/real/'
        runningExampleDataSet <- read.table(paste(path,filename,sep=""), sep=",", header=FALSE)

        print(runningExampleDataSet)

        rowCount <- nrow(runningExampleDataSet);
        colCount <- ncol(runningExampleDataSet);

        nameLabels <- unique(runningExampleDataSet[,colCount]);
        runningExampleDataSet[,colCount] <- as.numeric(runningExampleDataSet[,colCount])

        print(runningExampleDataSet)

        runningExampleDataMatrix <- as.matrix(runningExampleDataSet[,2:(colCount-1)]);
        resultLabels <- skinnyDipClusteringFullSpace(runningExampleDataMatrix);

        if (new == TRUE) {
                resultLabels <- clusterLabels(resultLabels, runningExampleDataMatrix);
        }

        labels <- generateAccuracyLabels(resultLabels, runningExampleDataSet[,colCount]);


        resultLabels[resultLabels == 0] <- 8

        classCols <- labels[,2]
        labels <- labels[,1]
        classCols[1] <- 8

        if (plotGT) {
                resultLabels <- runningExampleDataSet[,colCount];
        }
        resultLabels[resultLabels == 0] <- 8;

        # hard coded 2D plot
        plot(runningExampleDataMatrix[,1], runningExampleDataMatrix[,2], col=resultLabels, xlim=c(0,1), ylim=c(0,1));
        legend("bottomleft", labels, bg="white",  pch='o', col=classCols, cex=0.75)
        title("Accuracies in SkinnyDip Running Example")
        print(resultLabels)


}
