traverseHelp <-
function(mydata, place, treeLvl=1, maxTaxaDepth){
	REMOVELBL <- TRUE  	#set to false to show all labels, otherwise only non-zero labels will be shown
	newdata <- NULL		#data set of childern nodes
	childstr <- ""		#string passed back from child
	recstr <- "" 		#return string
	lvls <- levels(mydata[,1])	#names of different levels in the data
	lvlspt <- strsplit(as.character(lvls), ".", fixed = TRUE)	#the level names but split by "."
	siblings <- 0		#tracks number of siblings
	stopTree <- FALSE

	if(treeLvl >= maxTaxaDepth){
		stopTree <- TRUE
	}

	for(j in (1:nrow(mydata))){		#search for childern and create list of them
		if(place == j) next 		#skip self
		if(lvlspt[[place]] == lvlspt[[j]][1]){ #found a child, add to a list					
			temprow <- mydata[j,]
			templbl <- substring(lvls[j], nchar(lvlspt[[place]][1])+2, nchar(lvls[j]))
			temprow[1] <- templbl
			newdata <- rbind(newdata, temprow) #list of childern
		}

		if(is.na(lvlspt[[j]][2])) #sibling found
			siblings <- siblings + 1
	}

	if(!is.null(newdata) && !stopTree){ #childern found
		newdata[,1] <- factor(newdata[,1], labels = newdata[,1]) #set up new labels
		attr(newdata,"row.names") <- 1:nrow(newdata)
		lvls <- levels(newdata[,1]) 		#adjust length info to reflect newdata
		lvlspt <- strsplit(lvls, ".", fixed = TRUE)

		for(t in (1:nrow(newdata))){ 	#search through all rows
			if(length(lvlspt[[t]]) == 1){ 	#only look at top level nodes
				temp_hstr <- traverseHelp(newdata, t, treeLvl + 1, maxTaxaDepth)
				if(childstr != "")
					childstr <- paste(childstr, ",", temp_hstr, sep="")
				else
					childstr <- temp_hstr
			}
		}

		if(mydata[place,2] == 0) #set up string to be returned
			recstr <- paste("(", childstr, "):", mydata[place,2],  sep="")
		else
			recstr <- paste("(", childstr, ")", mydata[place,1], ":", mydata[place,2],  sep="")

		if(siblings == 0) #adds an extra 0 value branch if there are no siblings
			recstr <- paste(recstr, ",:0.0", sep="")
	}
	else{ #no childern found
		if(mydata[place,2] == 0 && REMOVELBL == TRUE) #set up string to be returned
			recstr <- paste(":", mydata[place,2], sep="")
		else
			recstr <- paste(mydata[place,1], ":", mydata[place,2], sep="")

		if(siblings == 0) #adds an extra 0 value branch if there are no siblings
			recstr <- paste(recstr, ",:0.0", sep="")
	}

	return(recstr)
}

