traverseTree <-
function(data, level="genus", split="."){
if(missing(data))
stop("A valid data set is required.")

if(any(grepl(")", rownames(data), fixed=TRUE)) || any(grepl("(", rownames(data), fixed=TRUE)) || any(grepl(":", rownames(data), fixed=TRUE)))
stop("Using parentheses and/or colons in the taxa names is not allowed.")

myStr <- ""
nameSplit <- strsplit(rownames(data), split, fixed=TRUE)
maxTaxaDepth <- getTaxaDepth(level)

for(i in 1:nrow(data)){ 
if(length(nameSplit[[i]]) == 1){ 
tempStr <- traverseTreeHelp(data[, 1, drop=FALSE], i, 1, maxTaxaDepth, split)
if(tempStr == "") 
next 
if(myStr != ""){
myStr <- paste(myStr, ",", tempStr, sep="")
}else{
myStr <- tempStr
}
}
}

myTree <- paste("", myStr, ";", sep="")

retTree <- read.tree(text=myTree) #turns the newick format into a tree
retTree <- collapse.singles(retTree)

return(retTree)
}
