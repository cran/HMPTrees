getTaxaDepth <-
function(myTaxaLevel="genus"){
	if(class(myTaxaLevel) != "character"){
		stop("'myTaxaLevel' must be a string of characters.")
	}
	if(tolower(myTaxaLevel) == "kingdom" || tolower(myTaxaLevel) == "k")
		return(1)
	else if(tolower(myTaxaLevel) == "phylum" || tolower(myTaxaLevel) == "p")
		return(2)
	else if(tolower(myTaxaLevel) == "class" || tolower(myTaxaLevel) == "c")
		return(3)
	else if(tolower(myTaxaLevel) == "order" || tolower(myTaxaLevel) == "o")
		return(4)
	else if(tolower(myTaxaLevel) == "family" || tolower(myTaxaLevel) == "f")
		return(5)
	else if(tolower(myTaxaLevel) == "genus" || tolower(myTaxaLevel) == "g")
		return(6)
	else if(tolower(myTaxaLevel) == "species" || tolower(myTaxaLevel) == "s")
		return(7)
	else
		stop(paste(myTaxaLevel, "isn't recognized."))
}

