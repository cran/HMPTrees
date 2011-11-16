getTopXPhylum <-
function(oldProbs, numPhyl){
	if(length(oldProbs) < numPhyl){ #we have too many probs
		stop("To many probabilities were given.")
	}else if(length(oldProbs) == numPhyl){
		return(c(1:numPhyl))
	}else{
		topX <- sort(oldProbs, decreasing=TRUE)[1:numPhyl]
		topPos <- NULL
		for(j in 1:numPhyl){
			for(i in 1:length(oldProbs)){
				if(topX[j] == oldProbs[i]){
					topPos <- c(topPos, i)
					break
				}
			}
		}
	}
	
	return(topPos)
}

