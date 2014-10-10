# problem si il n'y a pas de convergence !!!
coefplus<- function(object,data,R=999,...){
	if (missing(data)) 
		data <- object$data	
	sx <- summary(object,...)
	btx <- bootcoef(object=object,data=data,R=999)
	btcoef <- unlist(apply(btx$t,2,mean,na.rm=TRUE))
	btbias <- btcoef-btx$t0
	btse <- unlist(apply(btx$t,2,sd,na.rm=TRUE))
	coefbtx<- data.frame("bt coef"=btcoef,"bias"=btbias,"bt.se"=btse)
	coefbtx <- cbind(sx$coefficient,coefbtx)
	attr(coefbtx,"R") <- R
	return(coefbtx)	
}