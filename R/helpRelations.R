helpRelations <-
function(taxa, myLoc, parentLoc, relations){
	childFound <- FALSE
	childList <- NULL
	newdata <- vector('character', length(taxa))
	lvlspt <- strsplit(as.character(taxa), ".",  fixed = TRUE)	#the level names but split by "."
	
	for(j in (1:length(taxa))){		#search for childern and create list of them
		if(myLoc == j || taxa[j] == "") next 		#skip self
		if(lvlspt[[myLoc]] == lvlspt[[j]][1]){ #found a child, add to a list					
			newdata[j] <- substring(taxa[j], nchar(lvlspt[[myLoc]][1])+2, nchar(taxa[j]))
			childFound <- TRUE
		}
	}
	
	if(childFound){ #childern found
		lvlspt2 <- strsplit(as.character(newdata), ".",  fixed = TRUE)
		
		for(j in (1:length(newdata))){ 	#search through all rows
			if(length(lvlspt2[[j]]) == 1){ 	#only look at top level nodes
				childList <- c(childList, j)
				relations <- helpRelations(newdata, j, myLoc, relations)
			}
		}
	}
	
	if(is.null(childList)){
		relations[[myLoc]] <- 0
	}else{
		relations[[myLoc]] <- childList
	}
	
	return(relations)
}

