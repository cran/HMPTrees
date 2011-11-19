transformHMPtoHMPTree <-
function(data){
	data <- t(data)
	data <- cbind(taxa=rownames(data), data)
	
	return(data)
}

