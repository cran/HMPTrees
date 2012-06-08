generateTree <-
function(data=NULL, nreads=7000, nsamps=200, theta=0){
require(HMP)
if(is.null(data))
stop("A valid data set is required.")
if(!is.factor(data[1,1]))
stop("The first column must contain the levels of the data set to generate a tree.")
if(nreads <= 0)
stop("'nreads' must be positive and greater than 0.")

tempdata <- trimToTaxaLevel(data, "genus", eliminateParentNodes=FALSE)
tempdata <- transformHMPTreetoHMP(tempdata, elimZero=TRUE)

if(theta > 0 && theta < 1){
dirfit <- dirmult(tempdata) 
dirgamma <- dirfit$pi * ((1 - theta)/theta)
}else{
dirfit <- DM.MoM(tempdata)
dirgamma <- dirfit$gamma
}

gendata <- Dirichlet.multinomial(rep(nreads, nsamps), dirgamma)
colnames(gendata) <- colnames(tempdata)
gendata <- transformHMPtoHMPTree(gendata)

gendata <- merge(data[,1], gendata, by=1, all=TRUE)
gendata[is.na(gendata)] <- 0
gendata <- buildTree(gendata)

return(gendata)
}
