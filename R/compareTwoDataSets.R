compareTwoDataSets <-
function(data1, data2, numBootStraps=1000, enableMC = FALSE, cores = 8){
	if(missing(data1) || missing(data2)){
		stop("Two valid data sets are required.")
	}
	if(is.factor(data1[1,1]) && is.factor(data2[1,1])){#we can merge
		datacomb <- merge(data1, data2, by=1, all=TRUE)
		datacomb[is.na(datacomb)] <- 0 #set any NA's to 0
		
		d1max <- ncol(data1)
		d2max <- d1max + ncol(data2)-1
		
		data1 <- datacomb[,2:d1max]
		data2 <- datacomb[,(d1max+1):(d2max)]
	}else{
		if(nrow(data1) != nrow(data2)){#our data sets dont match and we cant merge
			stop("The data sets need to be the same length or have a column to merge on.")
		}
		
		if(is.factor(data1[1,1]))#strip off the taxa info if its there
			data1 <- data1[,-1]
		if(is.factor(data2[1,1]))
			data2 <- data2[,-1]
		
		data1[is.na(data1)] <- 0
		data2[is.na(data2)] <- 0		
	}
	
	#Observed LRT value
	datacomb <- cbind(data1, data2)
	fit1 <- getMLEandLoglike(data1)
	fit2 <- getMLEandLoglike(data2)
	fitcomb <- getMLEandLoglike(datacomb)
	LRTobs <- -2*(fitcomb$Loglik-fit1$Loglik-fit2$Loglik)
	if(LRTobs == 0){ #exactly the same
		return(1)
	}
	
	#Bootstrap: resampling with replacement
	bootsample <- 1:numBootStraps
	x1 <- ncol(data1)
	x2 <- ncol(data2)
	
	if(!enableMC){ #do the p value search sequentially 
		LRTbootstrap <- apply(as.matrix(bootsample), 1, function(x, x1, x2, datacomb){	
			p1 <- sample(x1+x2, replace=TRUE)
			data1b <- datacomb[,p1[c(1:x1)]]
			data2b <- datacomb[,p1[c((x1+1):(x1+x2))]]
			datacombb <- cbind(data1b, data2b)
			
			fit1 <- getMLEandLoglike(data1b)
			fit2 <- getMLEandLoglike(data2b)
			fitcomb <- getMLEandLoglike(datacombb)
			LRT <- -2*(fitcomb$Loglik-fit1$Loglik-fit2$Loglik)
		}, x1=x1, x2=x2, datacomb=datacomb)
	}else{ #add some parallelism to our calculations
		if(Sys.info()[[1]] != "Linux"){
			stop("A Linux OS is required for multicore support due to additonal package use.")
		}
		require(doSMP)
		require(foreach)
		require(multicore)
		require(doMC)
		
		registerDoMC(cores = cores)
		
		chunkSize <- ceiling(numBootStraps/getDoParWorkers())
		smpopts <- list(chunkSize = chunkSize)
		LRTbootstrap <- foreach(icount(numBootStraps), .combine = cbind, .options.smp = smpopts) %dopar%{
			p1 <- sample(x1+x2, replace=TRUE)
			data1b <- datacomb[,p1[c(1:x1)]]
			data2b <- datacomb[,p1[c((x1+1):(x1+x2))]]
			datacombb <- cbind(data1b, data2b)
			
			fit1 <- getMLEandLoglike(data1b)
			fit2 <- getMLEandLoglike(data2b)
			fitcomb <- getMLEandLoglike(datacombb)
			LRT <- -2*(fitcomb$Loglik-fit1$Loglik-fit2$Loglik)
		}
	}
	
	#P-value
	p.value <- sum(LRTbootstrap > LRTobs)/numBootStraps	
	return(p.value)
}

