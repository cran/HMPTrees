helpBuildTree <-
function(data, taxa, myLoc){
childFound <- FALSE
newdata <- vector('character', length(taxa))
lvlspt <- strsplit(as.character(taxa), ".",  fixed = TRUE)#the level names but split by "."

for(j in 1:length(taxa)){#search for childern and create list of them
if(myLoc == j || taxa[j] == "") next #skip self
if(lvlspt[[myLoc]] == lvlspt[[j]][1]){ #found a child, add to a list
newdata[j] <- substring(taxa[j], nchar(lvlspt[[myLoc]][1])+2, nchar(taxa[j]))
childFound <- TRUE
}
}

if(childFound){ #childern found
lvlspt <- strsplit(as.character(newdata), ".",  fixed = TRUE)

for(j in 1:length(newdata)){ #search through all rows
if(length(lvlspt[[j]]) == 1){ #only look at top level nodes
data <- helpBuildTree(data, newdata, j)
data[myLoc,-1] <- data[myLoc,-1] + data[j,-1]
}
}
}

return(data)
}
