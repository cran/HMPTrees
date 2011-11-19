trimToTaxaLevel <-
function(data, myTaxaLevel="genus", eliminateParentNodes=FALSE){
	if(missing(data)){
		stop("A valid data set is required.")
	}
	
	maxLevel <- getTaxaDepth(myTaxaLevel)
	
	clvlspt <- strsplit(as.character(data[,1]), ".", fixed = TRUE)#names of levels split by " "
	data[,1] <- as.character(data[,1])
	lowerLevels <- NULL
	for(l in (1:nrow(data))){ 	#search through all rows
		if(length(clvlspt[[l]]) == maxLevel && eliminateParentNodes){
			lowerLevels <- c(lowerLevels, l)
			data[l,1] <- clvlspt[[l]][maxLevel]
		}else if(length(clvlspt[[l]]) >= maxLevel && !eliminateParentNodes){
			lowerLevels <- c(lowerLevels, l)
		}
	}
	data <- data[lowerLevels,]
	data[,1] <- as.factor(data[,1])
	return(data)
}

