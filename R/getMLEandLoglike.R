getMLEandLoglike <-
function(data, maxSteps=50, weight=NULL){
if(missing(data)){
stop("A valid data set is required.")
}
taxa <- NULL
if(is.factor(data[1,1])){#strip off the taxa info if its there
taxa <- data[1] 
data <- data[,-1]
}

if(is.null(weight)){
gstart <- apply(data, 1, mean)
}else{
gstart <- apply(data, 1, function(x){p=(x%*%weight)/sum(weight)})
}

while(rowMatch(t(data),t(gstart))[1] != 0){ #make sure the mean isnt a point in our data set
gstart <- gstart+0.01
}

#Iterative Procedure
ret <- data.frame(matrix(0, nrow=maxSteps, ncol=2))
names(ret) <- c("f", "deltaf")
calc.f <- sum(apply(data, 2, function(x,gstart){sqrt(sum((x-gstart)^2))}, gstart=gstart))
delta.f <- 1
count <- 0

while((count < maxSteps) && (delta.f > 10^(-6))){ 
count <- count+1
ret[count,] <- c(calc.f, delta.f)
if(is.null(weight)){
gstart <- rowSums(apply(data, 2, function(x, gstart){x/sqrt(sum((x-gstart)^2))}, gstart=gstart))/sum(apply(data, 2, function(x,gstart){1/sqrt(sum((x-gstart)^2))}, gstart=gstart))
calc.f <- sum(apply(data, 2, function(x, gstart){sqrt(sum((x-gstart)^2))}, gstart=gstart))
}else{
gstart <- (apply(data, 2, function(x, gstart){x/sqrt(sum((x-gstart)^2))}, gstart=gstart)%*%weight)/as.vector(apply(data, 2, function(x,gstart){1/sqrt(sum((x-gstart)^2))}, gstart=gstart)%*%weight)
calc.f <- as.vector(apply(data, 2, function(x, gstart){sqrt(sum((x-gstart)^2))}, gstart=gstart)%*%weight)
}
delta.f <- abs(ret[count,1]-calc.f)
}
#MLE tau
N <- ncol(data)
p <- nrow(data)
if(is.null(weight)){
tau <- N/calc.f
}else{
tau <- sum(weight)/calc.f
}

if(!is.null(taxa)){#bind the taxa back on if we took it off
gstart <- cbind(taxa, gstart)
}

#Loglikelihood
if(is.null(weight)){
log.likelihood <- N*(lgamma(p/2)-lgamma(p)+log(tau)-p*log(2)-p/2*log(pi))-tau*calc.f
}else{
log.likelihood = sum(weight)*(lgamma(p/2)-lgamma(p)+log(tau)-p*log(2)-p/2*log(pi))-tau*calc.f
}
mle.fit <- list(count, log.likelihood, tau, as.data.frame(gstart))
names(mle.fit) <- c("iters", "Loglik", "tau", "mleTree")
return(mle.fit)
}
