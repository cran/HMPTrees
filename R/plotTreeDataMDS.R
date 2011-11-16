plotTreeDataMDS <-
function(data1=NULL, data2=NULL, data3=NULL, myTitle="Tree MDS Comparisons", calcMLE=TRUE, mleTitle1="Data1 MLE", 
		mleTitle2="Data2 MLE", mleTitle3="Data3 MLE"){
	myPSize <- 1
	myPColors <- NULL
	myPColors2 <- NULL
	mleTitles <- NULL
	dataCount <- 0
	
	if(is.null(data1) && is.null(data2) && is.null(data3)){ #make sure we have at least 1 data set
		stop("At least one valid data set is required.")
	}
	
	if(!is.null(data1)){
		sub <- 0
		if(is.factor(data1[1,1]))
			sub <- 1
		if(ncol(data1) < (1+sub))
			stop("'Data1' is not valid.")
		myPColors <- c(myPColors, rep('red', (ncol(data1)-sub)))
		myPColors2 <- c(myPColors2, 'green')
		dataCount <- dataCount + 1
		mleTitles <- c(mleTitles, mleTitle1)
	}
	if(!is.null(data2)){
		sub <- 0
		if(is.factor(data2[1,1]))
			sub <- 1
		if(ncol(data2) < (1+sub))
			stop("'Data2' is not valid.")
		myPColors <- c(myPColors, rep('blue', (ncol(data2)-sub)))
		myPColors2 <- c(myPColors2, 'orange')
		dataCount <- dataCount + 1
		mleTitles <- c(mleTitles, mleTitle2)
	}
	if(!is.null(data3)){
		sub <- 0
		if(is.factor(data3[1,1]))
			sub <- 1
		if(ncol(data3) < (1+sub))
			stop("'Data3' is not valid.")
		myPColors <- c(myPColors, rep('cyan', (ncol(data3)-sub)))
		myPColors2 <- c(myPColors2, 'purple')
		dataCount <- dataCount + 1
		mleTitles <- c(mleTitles, mleTitle3)
	}
		
	data <- mergeDataSets(data1, data2, data3, calcMLE)

	if(class(data[1,1]) == "factor"){
		tdata <- t(data[,-1])
	}else{
		tdata <- t(data)
	}
	tdata.dist <- dist(tdata)
	loc <- cmdscale(tdata.dist, k=2)
	x <- loc[,1]
	y <- -loc[,2]
	
	myPColors <- c(myPColors, myPColors2, 'black')
	plot(x, y, pch=19, xlab='MDS 1', ylab='MDS 2', pty='s', col=myPColors, cex=myPSize, main=myTitle)
	
	if(calcMLE){ #only plot mle titles if we are plotting them
		if(dataCount > 1){ #dont add a combined mle if we dont make one
			mleTitles <- c(mleTitles, "Combined MLE")
			dataCount <- dataCount + 1
		}
		text(x[(nrow(tdata)-dataCount+1):nrow(tdata)], y[(nrow(tdata)-dataCount+1):nrow(tdata)], mleTitles, pos=3, cex=.75)
	}
}

