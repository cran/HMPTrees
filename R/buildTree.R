buildTree <-
function(data=NULL){
if(is.null(data))
stop("A valid data set is required.")
if(!is.factor(data[1,1]))
stop("The first column must contain the levels of the data set.")

taxa <- data[1]

clvls <- levels(taxa[,1])#names of levels in data set
clvlspt <- strsplit(as.character(clvls), ".",  fixed = TRUE)#names of levels split by " "

for(i in 1:nrow(data)){ #search through all rows
if(length(clvlspt[[i]]) == 1){ #only look at top level nodes
data <- helpBuildTree(data, clvls, i)
}
}

return(data)
}
