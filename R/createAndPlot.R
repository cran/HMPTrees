createAndPlot <-
function(data=NULL, sampNum=2, allSamp=FALSE, maxTaxaLevel="genus", myColors, 
		myDivisions, myTitle, mySubTitle, showTipLabel=TRUE, showNodeLabel=FALSE, displayLegend=TRUE, onePerPage=FALSE){
	
	if(is.null(data) || ncol(as.data.frame(data)) < 2){
		stop("A valid data set is required.")
	}
	if(!is.factor(data[1,1]))
		stop("The first column must contain the levels of the data set to plot it.")
	
	trees <- createTrees(data, sampNum, allSamp, maxTaxaLevel)
	
	if(onePerPage){
		par(layout(1))
	}else{ #4 per page
		par(layout(matrix(c(1,3,2,4), 2, 2)))
	}
	plotTree(trees, myColors, myDivisions, myTitle, mySubTitle, 
			showTipLabel, showNodeLabel, displayLegend)
	
	par(layout(1))
}

