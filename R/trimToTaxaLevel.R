trimToTaxaLevel <-
function(data, myTaxaLevel="genus", eliminateParentNodes=FALSE, trimBelow = NULL){
if(missing(data)){
stop("A valid data set is required.")
}
if(!is.factor(data[1,1]))
stop("The first column must contain the levels of the data set.")

maxLevel <- getTaxaDepth(myTaxaLevel)

clvlspt <- strsplit(as.character(data[,1]), ".", fixed = TRUE)#names of levels split by "."
data[,1] <- as.character(data[,1])
lowerLevels <- NULL
for(l in (1:nrow(data))){ #search through all rows
if(length(clvlspt[[l]]) == maxLevel){
lowerLevels <- c(lowerLevels, l)
if(eliminateParentNodes && is.null(trimBelow))
data[l,1] <- clvlspt[[l]][maxLevel]
}else if(!is.null(trimBelow) && is.logical(trimBelow)){
if(length(clvlspt[[l]]) < maxLevel && trimBelow){
lowerLevels <- c(lowerLevels, l)
}else if(length(clvlspt[[l]]) > maxLevel && !trimBelow){
lowerLevels <- c(lowerLevels, l)
}
}
}
data <- data[lowerLevels,]
data[,1] <- as.factor(data[,1])
return(data)
}
