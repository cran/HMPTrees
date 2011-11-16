displayLegend <-
function(brColors, divisions, title="Confidence Value"){
	if(missing(divisions)){ #default divisions
		divisions <- c(.1, 1, 10, 100, 1000, 10000)
	}
	sort(divisions)
	divisions <- c(0, divisions)#always want the 0 lengths to be white
	if(missing(brColors)){ #default colors
		brColors <- c("red", "orange", "yellow", "green" , "cyan", "blue")
	}
	
	if(length(divisions) > (length(brColors)+1)){ #need more colors, dont care if more colors than divisons
		brColors <- c(brColors, rep(brColors[length(brColors)], (length(divisions) - length(brColors)-1)))
	}
	
	lgd <- NULL
	for(num in length(divisions):2){
		lgd <- c(lgd, paste(divisions[num], "-", divisions[num-1], sep=""))
	}
	
	palette(brColors)
	plot(0,0, type ="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
	legend(-.75,.75,legend=lgd, col=rev(palette()), pch=19, title=title)
}

