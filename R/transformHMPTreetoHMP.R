transformHMPTreetoHMP <-
function(data, elimZero=FALSE, zeroValue=.00001){
if(missing(data)){
stop("A valid data set is required.")
}

rownames(data) <- data[,1]
data <- data[,-1]

#remove rows of all 0's
yy <- apply(data, 1, sum)
counts <- NULL
for(r in nrow(data):1){
if(yy[[r]] == 0){
counts <- c(counts, r)
data[r,2] <- zeroValue
}
}
if(elimZero)
data <- data[-counts,]

return(t(data))
}
