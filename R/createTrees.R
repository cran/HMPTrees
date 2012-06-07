createTrees <-
function(data=NULL, sampNum=2, allSamp = FALSE, maxTaxaLevel = "genus"){
if(is.null(data) || ncol(as.data.frame(data)) < 2)
stop("A valid data set is required.")
if(!is.factor(data[1,1]))
stop("The first column must contain the levels of the data set to create a phylo object.")

if(allSamp)
sampNum <- c(2:ncol(data))

data[,1] <- factor(data[,1], labels = levels(factor(data[,1])))
rownames(data) <- 1:nrow(data)

data <- data[order(data[,1]),]

allTrees <- vector("list", length(sampNum))
i <- 1
for(q in sampNum){ #run through all samples
if(as.numeric(q) > ncol(data)){ #make sure we got a good number
stop(paste(q," is larger than the bounds of the data set",sep=""))
}
if(as.numeric(q) <= 1){ #make sure we got a good number
stop(paste(q," is smaller than the bounds of the data set",sep=""))
}
oneDay <- data[as.numeric(q)] #a sample from 1 day

if((max(as.numeric(data[,1])) > 1)){#skips entries without data
theData <- cbind(data[1], oneDay) #breaks the data into a 2 column data set
tr <- traverseTree(theData, maxTaxaLevel)
}
allTrees[[i]] <- tr
i <- i + 1
}

return(allTrees)
}
