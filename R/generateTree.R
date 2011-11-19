generateTree <-
function(data=NULL, maxScore=7000, equalProb=TRUE, setProbs){
	probs <- NULL
	if(is.null(data) || ncol(as.data.frame(data)) < 2)
		stop("A valid data set is required.")
	if(!is.factor(data[1,1]))
		stop("The first column must contain the levels of the data set to generate a tree.")
	
	if(ncol(data) > 1){ #reduce datas size
		probs <- data[2]
		data <- data[1]
	}else{
		probs <- data.frame(probs=matrix(1, nrow(data)))
		data <- data[1]
	}
	
	relations <- getRelations(data)
	data$values <- 0 #add a blank column in
	
	for(i in 1:maxScore){
		myLoc <- 1
		childLoc <- 2
		
		while(childLoc != 0){ #loop through each branch until we hit an end node
			data[myLoc, 2] <- data[myLoc, 2] + 1
			
			if(length(relations[[myLoc]]) == 1){ #only 1 child
				childLoc <- relations[[myLoc]]
			}else{ #randomly choose a child node
				if(myLoc == 1 && !missing(setProbs)){
					p <- getTopXPhylum(probs[relations[[myLoc]],], length(setProbs))
					childLoc <- sample(relations[[myLoc]][p], 1, prob=setProbs)
				}else{
					if(!equalProb)
						childLoc <- sample(relations[[myLoc]], 1, prob=probs[relations[[myLoc]],])
					else
						childLoc <- sample(relations[[myLoc]], 1)
				}
			}
			
			myLoc <- childLoc
		}
	}
	
	return(data)
}

