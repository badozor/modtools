# cost function for multiple categories
costDA2 <- function(y,yhat){
	tmp <- table(y,yhat)
	sum(diag(tmp))/sum(tmp)
}
costKappa2 <- function(y,yhat,...){
	kappa2(cbind(y,yhat),...)$value
}
# see Faraway 2006, extending the linear model with R.
costEntropy <- function(x){
	w <- as.vector(as.matrix(x))
	-sum(w*log(ifelse(w==0,1,w)))
}
costGini <- function(x){
	w <- as.vector(as.matrix(x))
	1-sum(w**2)
}
