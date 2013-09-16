checkTreeValidHelp <-
function(data, place, epsilon, split){
myName <- rownames(data)[place]
myVal <- data[place, 1]

data <- data[-place,, drop=FALSE] 
childRows <- grep(paste(myName, split, sep=""), rownames(data), fixed=TRUE)

if(length(childRows) != 0){ #we know we have children
newData <- data[childRows,, drop=FALSE]
rownames(newData) <- substring(rownames(newData), nchar(myName)+2, nchar(rownames(newData)))
nameSplit <- strsplit(rownames(newData), split, fixed=TRUE)

childCount <- 0
for(t in 1:nrow(newData)){ 
if(length(nameSplit[[t]]) == 1){ 
childCount <- childCount + newData[t, 1]
if((myVal+epsilon) < childCount) #the child is greater so bad tree
return(FALSE)
valid <- checkTreeValidHelp(newData, t, epsilon, split)
if(!valid)
return(FALSE)
}
}
}

return(myVal >= 0) #check bottom nodes arent negative
}
