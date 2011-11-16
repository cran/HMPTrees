checkValidHelp <-
function(mydata, place, epsilon){
	newdata <- NULL
	lvls <- levels(mydata[,1])	#names of levels in data set
	lvlspt <- strsplit(as.character(lvls), ".", fixed = TRUE)#names of levels split by " "
	myCount <- mydata[place, 2]
	
	for(j in (1:nrow(mydata))){		#search for children and create list of them
		if(place == j) next 		#skip self
		if(lvlspt[[place]] == lvlspt[[j]][1]){ #found a child, add to a list					
			temprow <- mydata[j,]
			templbl <- substring(lvls[j], nchar(lvlspt[[place]][1])+2, nchar(lvls[j]))
			temprow[1] <- templbl
			newdata <- rbind(newdata, temprow) #list of childern
		}
	}
	
	if(!is.null(newdata)){ #children found
		newdata[,1] <- factor(newdata[,1], labels = newdata[,1]) #set up new labels
		attr(newdata,"row.names") <- 1:nrow(newdata)
		lvls <- levels(newdata[,1]) 		#adjust length info to reflect newdata
		lvlspt <- strsplit(lvls, ".", fixed = TRUE)
		
		childCount <- 0
		for(t in (1:nrow(newdata))){ 	#search through all rows
			if(length(lvlspt[[t]]) == 1){ 	#only look at top level nodes
				childCount <- childCount + newdata[t, 2]#temp
				if((myCount+epsilon) < childCount){ #the child is greater so bad tree
					return(FALSE)
				}
				valid <- checkValidHelp(newdata, t,epsilon)
				if(!valid){
					return(FALSE)
				}
			}
		}
	}else{	
		return((mydata[place,2] >= 0)) #check bottom nodes arent negative
	}
	
	return(valid)
}

