getBranchSizes <-
function(trdata, brColors, divisions){
if(missing(trdata) || class(trdata) != "phylo"){
stop("A valid tree is required.")
}

edgecol <- NULL
edgewid <- NULL
edgelen <- trdata$edge.length

noZdata <- subset(edgelen, edgelen != 0)#get min and max without 0's in data set
quan <- quantile(noZdata, probs = seq(0, 1, 0.2))

if(missing(divisions)){ #default divisions
divisions <- c(.1, 1, 10, 100, 1000, 10000)
}
sort(divisions)
divisions <- c(quan[[1]], divisions)#always want the 0 lengths to be white

if(missing(brColors)){ #default colors
brColors <- c("red", "orange", "yellow", "green" , "cyan", "blue")
}

if(length(divisions) > (length(brColors)+1)){ #need more colors, dont care if more colors than divisons
brColors <- c(brColors, rep(brColors[length(brColors)], (length(divisions) - length(brColors)-1)))
}

palette(brColors)

for(i in (1:length(edgelen))){ #loop through all tree edges
if(edgelen[as.numeric(i)] < divisions[1]){ #0 value so make it white
edgecol <- append(edgecol, 0)
edgewid <- append(edgewid, 0)
}else{
for(j in (2:length(divisions))){
if(edgelen[as.numeric(i)] <= divisions[j]){
cval <- (j-1)
len <- floor(4*j/length(divisions)) 
edgecol <- append(edgecol, cval)
edgewid <- append(edgewid, len)
break
}
}
}
}

bs <- list(edgecol = edgecol, edgewid = edgewid)
return(bs)
}
