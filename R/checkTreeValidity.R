checkTreeValidity <-
function(data = NULL, sample = 2, checkAll=FALSE, epsilon = 0.0001){
	overAllValid <- NULL
	if(is.null(data) || ncol(as.data.frame(data)) < 2){
		stop("A valid data set is required.")
	}
	if(!is.factor(data[1,1]))
		stop("The first column must contain the levels of the data set.")
	if(checkAll){
		sample <- c(2:ncol(data))
	}
	
	for(samp in sample){
		if(as.numeric(samp) > ncol(data)){ #make sure we got a good number
			stop(paste(samp," is larger than the bounds of the data set",sep=""))
		}
		if(as.numeric(samp) <= 1){ #make sure we got a good number
			stop(paste(samp," is smaller than the bounds of the data set",sep=""))
		}
		oneDay <- data[as.numeric(samp)] #a sample from 1 day
		tempdata <- cbind(data[1], oneDay)
		
		clvls <- levels(tempdata[,1])	#names of levels in data set
		clvlspt <- strsplit(as.character(clvls), ".", fixed = TRUE)#names of levels split by " "
		
		for(i in (1:nrow(tempdata))){ 	#search through all rows
			if(length(clvlspt[[i]]) == 1){ #only look at top level nodes - should only be 1		
				valid <- checkValidHelp(tempdata, i, epsilon)
				if(!valid){
					print(paste("Tree", samp, "is invalid.",sep=" "))
				}
			}
		}
		overAllValid <- c(overAllValid, valid)
	}
	return(overAllValid)
}

