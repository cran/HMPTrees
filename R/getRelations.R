getRelations <-
function(data=NULL){
	taxa <- NULL
	if(is.null(data))
		stop("A valid data set is required.")
	if(!is.factor(data[1,1]))
		stop("The first column must contain the levels of the data set.")
	
	taxa <- data[1]
	data <- NULL
	
	clvls <- levels(taxa[,1])	#names of levels in data set
	clvlspt <- strsplit(as.character(clvls), ".",  fixed = TRUE)#names of levels split by " "
	relations <- NULL
	relations[[1]] <- c(1, 2, 3)
	
	for(i in (1:dim(taxa)[1])){ 	#search through all rows
		if(length(clvlspt[[i]]) == 1){ #only look at top level nodes
			relations <- helpRelations(clvls, i, 0, relations)
		}
	}
	
	return(relations)
}

