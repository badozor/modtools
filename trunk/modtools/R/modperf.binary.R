modperf.binary <- function(x,...){
	if(!inherits(x,"glm"))
		stop("non convenient argument!")
	n <- length(x$y)
	p <- n-x$df.residual-1
	aic1 <- AIC(x,k=2)
	bic1 <- AIC(x,k=log(n))
	aicc1 <- aic1 + 2*p*(p+1)/(n-p-1)
	res2 <- rstandard(x)^2
	CovPat <- sum(as.numeric(res2>4))/length(res2)
	X2 <- sum(residuals(x,type="pearson")^2)
	df <- df.residual(x)
	p.value <- 1- pchisq(X2,df)
	w <- prep.roc(x$y,predict(x,type="response"),...)
	w <- summary(w,display=FALSE)
	res <- c(n,p,CovPat,aic1,aicc1,bic1,X2,df,p.value,unlist(w))
	names(res) <- c("n","p","CovPat","AIC","AICc","BIC","X2","df","p.value",names(w))
	return(res)
}