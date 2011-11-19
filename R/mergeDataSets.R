mergeDataSets <-
function(data1=NULL, data2=NULL, data3=NULL, calcMLE=TRUE){
	dataCount <- 1
	taxa1 <- FALSE
	taxa2 <- FALSE
	taxa3 <- FALSE
	taxa <- NULL
	sub <- 0
	
	if(is.factor(data1[1,1]))
		taxa1 <- TRUE
	if(is.factor(data2[1,1]))
		taxa2 <- TRUE
	if(is.factor(data3[1,1]))
		taxa3 <- TRUE
	
	if((taxa1 || is.null(data1)) && (taxa2 || is.null(data2)) && (taxa3 || is.null(data3)))
		sub <- 1
	
	if((!is.null(data1) && !is.null(data2) && !is.null(data3)) && !((ncol(data1) < (1+sub)) && (ncol(data2) < (1+sub)) && (ncol(data3) < (1+sub)))){ #all 3 present
		dataCount <- 3
		if(taxa1 && taxa2 && taxa3){
			data <- merge(data1, data2, by=1, all=TRUE)
			data <- merge(data, data3, by=1, all=TRUE)
			taxa <- data[,1]
			data <- data[,-1]
			data[is.na(data)] <- 0 #set any NA's to 0
			
			if(calcMLE){
				d1max <- ncol(data1) - 1
				d2max <- d1max + ncol(data2) - 1
				d3max <- d2max + ncol(data3) - 1		
				data1 <- data[,1:d1max]
				data2 <- data[,(d1max+1):(d2max)]
				data3 <- data[,(d2max+1):(d3max)]
			}
		}else{
			if(taxa1)
				data1 <- data1[,-1]
			if(taxa2)
				data2 <- data2[,-1]
			if(taxa3)
				data3 <- data3[,-1]
			
			if(nrow(data1) != nrow(data2) || nrow(data1) != nrow(data3))
				stop("The data sets need to have the same number of rows or be able to merge on the first column.")
			data <- cbind(data1, data2, data3)
			data[is.na(data)] <- 0 #set any NA's to 0
		}
	}else if((!is.null(data1) && !is.null(data2)) && !((ncol(data1) < (1+sub)) && (ncol(data2) < (1+sub)))){ #only 1 and 2	
		dataCount <- 2
		if(taxa1 && taxa2){
			data <- merge(data1, data2, by=1, all=TRUE)
			taxa <- data[,1]
			data <- data[,-1]
			data[is.na(data)] <- 0 #set any NA's to 0
			
			if(calcMLE){
				d1max <- ncol(data1) - 1
				d2max <- d1max + ncol(data2) - 1
				data1 <- data[,1:d1max]
				data2 <- data[,(d1max+1):(d2max)]
			}
		}else{
			if(taxa1)
				data1 <- data1[,-1]
			if(taxa2)
				data2 <- data2[,-1]

			if(nrow(data1) != nrow(data2))
				stop("The data sets need to have the same number of rows or be able to merge on the first column.")
			data <- cbind(data1, data2)
			data[is.na(data)] <- 0 #set any NA's to 0
		}
	}else if((!is.null(data1) && !is.null(data3)) && !((ncol(data1) < (1+sub)) && (ncol(data3) < (1+sub)))){ #only 1 and 3
		dataCount <- 2
		if(taxa1 && taxa3){
			data <- merge(data1, data3, by=1, all=TRUE)
			taxa <- data[,1]
			data <- data[,-1]
			data[is.na(data)] <- 0 #set any NA's to 0
			
			if(calcMLE){
				d1max <- ncol(data1) - 1
				d3max <- d1max + ncol(data3) - 1
				data1 <- data[,1:d1max]
				data3 <- data[,(d1max+1):(d3max)]
			}
		}else{
			if(taxa1)
				data1 <- data1[,-1]
			if(taxa3)
				data3 <- data3[,-1]
			
			if(nrow(data1) != nrow(data3))
				stop("The data sets need to have the same number of rows or be able to merge on the first column.")
			data <- cbind(data1, data3)
			data[is.na(data)] <- 0 #set any NA's to 0
		}
	}else if((!is.null(data2) && !is.null(data3)) && !((ncol(data2) < (1+sub)) && (ncol(data3) < (1+sub)))){ #only 2 and 3
		dataCount <- 2
		if(taxa2 && taxa3){
			data <- merge(data2, data3, by=1, all=TRUE)
			taxa <- data[,1]
			data <- data[,-1]
			data[is.na(data)] <- 0 #set any NA's to 0
			
			if(calcMLE){
				d2max <- ncol(data2) - 1
				d3max <- d2max + ncol(data3) - 1
				data2 <- data[,1:d2max]
				data3 <- data[,(d2max+1):(d3max)]
			}
		}else{
			if(taxa2)
				data2 <- data2[,-1]
			if(taxa3)
				data3 <- data3[,-1]
			
			if(nrow(data2) != nrow(data3))
				stop("The data sets need to have the same number of rows or be able to merge on the first column.")
			data <- cbind(data2, data3)
			data[is.na(data)] <- 0 #set any NA's to 0
		}
	}else if(!is.null(data1) && !((ncol(data1) < (1+sub)))){ #only 1
		if(taxa1){
			taxa <- data1[,1]
			data1 <- data1[,-1]
			data <- data1		
		}else{
			data <- data1
		}
	}else if(!is.null(data2) && !((ncol(data2) < (1+sub)))){ #only 2
		if(taxa2){
			taxa <- data2[,1]
			data2 <- data2[,-1]
			data <- data2
		}else{
			data <- data2
		}
	}else if(!is.null(data3) && !((ncol(data3) < (1+sub)))){ #only 3
		if(taxa3){
			taxa <- data3[,1]
			data3 <- data3[,-1]
			data <- data3
		}else{
			data <- data3
		}
	}else{
		stop("At least one valid data set is required.")
	}
	
	if(calcMLE){
		mleAll <- getMLEandLoglike(data)$mleTree
		colnames(mleAll) <- "combined mle"
		if(!is.null(data1) && !(ncol(data1) < (1+sub))){
			mle <- getMLEandLoglike(data1)$mleTree
			colnames(mle) <- "data1mle"
			data <- cbind(data, mle)
		}
		if(!is.null(data2)&& !(ncol(data2) < (1+sub))){
			mle <- getMLEandLoglike(data2)$mleTree
			colnames(mle) <- "data2mle"
			data <- cbind(data, mle)
		}
		if(!is.null(data3)&& !(ncol(data3) < (1+sub))){
			mle <- getMLEandLoglike(data3)$mleTree
			colnames(mle) <- "data3mle"
			data <- cbind(data, mle)
		}
		if(dataCount > 1){
			data <- cbind(data, mleAll)
		}
		
		data[is.na(data)] <- 0 #set any NA's to 0
	}
	
	if(!is.null(taxa))
		data <- cbind(taxa, data)
	
	return(data)
}

