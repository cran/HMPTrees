formatData <-
function(data, countThreshold=1000, normalizeThreshold=10000){
	if(missing(data))
		stop("A valid data set is required.")
	if(class(countThreshold) != "numeric")
		stop("'countThreshold' must be numeric.")
	if(class(normalizeThreshold) != "numeric")
		stop("'normalizeThreshold' must be numeric.")
	
	for(i in ncol(data):1){#drop low and null samples
		if(class(data[1,i]) != "numeric"){#skip columns that arent numbers
			next
		}
		if(data[1,i] < countThreshold || max(data[,i]) == 0){ 
			data[,i] <- NULL
			next
		}
		if(normalizeThreshold > 0){ #normalize if we have a number greater than 0
			data[,i] <- data[,i] * (normalizeThreshold/data[1,i])
		}
	}
	
	data[,1] <- factor(data[,1], labels = levels(factor(data[,1]))) #format data
	attr(data, "row.names") <- 1:nrow(data)
	data[is.na(data)] <- 0
	data <- data[order(data[,1]),]
	
	return(data)
}

