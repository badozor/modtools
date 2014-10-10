modperf.glm <-
function(x,...){
  if(!inherits(x,"glm"))
    stop("non convenient argument!")
  n <- length(x$y)
  p <- n-x$df.residual
  aic1 <- AIC(x,k=2)
  bic1 <- AIC(x,k=log(n))
  aicc1 <- aic1 + 2*p*(p+1)/(n-p-1)
  R2 <- summary(lm(x$y~x$fitted))$r.squared
  X2 <- sum(residuals(x,type="pearson")^2)
  df <- df.residual(x)
  p.value <- 1- pchisq(X2,df)
  res <- c(n,p,R2,aic1,aicc1,bic1,X2,df,p.value)
  names(res) <- c("n","p","R2","AIC","AICc","BIC","X2","df","p.value")
 return(res)
}
