gof.test <- function(x, ...){
	X2 <- sum(residuals(x,type="pearson")^2)
	df <- df.residual(x)
	pvalue <- 1-pchisq(X2,df)		
	w <- c(X2,df,pvalue)
	names(w) <-  c("X2","df","pvalue")
	w
}
