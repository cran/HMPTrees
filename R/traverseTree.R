traverseTree <-
function(data, maxTaxaLevel="genus"){
	if(missing(data)){
		stop("A valid data set is required.")
	}
	
	mystr <- ""		#string passed back from craet_help
	clvls <- levels(data[,1])	#names of levels in data set
	clvlspt <- strsplit(as.character(clvls), ".", fixed = TRUE)#names of levels split by " "
	treeLvl <- 1
	
	maxTaxaDepth <- getTaxaDepth(maxTaxaLevel);

	for(i in (1:nrow(data))){ 	#search through all rows
		if(length(clvlspt[[i]]) == 1){ #only look at top level nodes
			temp_cstr <- traverseHelp(data, i, treeLvl, maxTaxaDepth)
			if(temp_cstr == "") next  #skip any empty returns
			if(mystr != "")
				mystr <- paste(mystr, ",", temp_cstr, sep="")
			else
				mystr <- temp_cstr
		}
	}

	mytree <- paste("", mystr, ";", sep="")

	tr <- read.tree(text = mytree) #turns the newick format into a tree
	tr <- collapse.singles(tr)
	
	return(tr)
}

