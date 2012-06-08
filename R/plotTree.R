plotTree <-
function(trees, myColors, myDivisions, myTitle, mySubTitle, 
showTipLabel=TRUE, showNodeLabel=FALSE, displayLegend=TRUE){

if(missing(trees)){
stop("A valid tree of type 'phylo' is required.")
}
count <- 1

if(displayLegend){
displayLegend(myColors, myDivisions)
}

for(tr in trees){
bs <- getBranchSizes(tr, myColors, myDivisions)

par(cex = .75)
plot(tr, type = "r", root.edge = FALSE, edge.color = bs$edgecol, edge.width = bs$edgewid, 
show.tip.label=showTipLabel, show.node.label=showNodeLabel) #plots the tree

if(missing(myTitle)){
theTitle <- paste("Sample: ", count, sep="")
count <- count + 1
}else{
theTitle <- myTitle
}
if(missing(mySubTitle)){
theSubTitle <- ""
}else{
theSubTitle <- mySubTitle
}

title(theTitle, sub = theSubTitle)
}
}
