subsetData <-
function(data=NULL, mySite = "saliva", myRegion = "v3-v5"){
data <- subset(data, data[1] == mySite)
data[1] <- NULL
data <- subset(data, data[1] == myRegion) 
data[1] <- NULL
return(data)
}
