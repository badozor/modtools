plotLeverage <- function(object,type="pearson",sub="Leverage",cex=1.5,pch=20,...){
  if(!inherits(object,"glm"))
    stop("non convenient argument")
  # leverage plot
  respearson <- residuals.glm(object,type=type)
  n <- length(object$y)
  p <- n-object$df.residual
  infl1 <- influence(object)
  hat1 <- infl1$hat
  plot(hat1,respearson,type="h",xlab="leverage",ylab="Residuals",col="red",panel.first=c(grid()))
  points(hat1,respearson,pch=pch,cex=cex,col="red")
  abline(v=3*p/n,lty=2)
  abline(v=2*p/n,lty=2)
  abline(h=c(-1.64,1.64),lty=2)
  abline(h=0,lty=2,lwd=2)
  title(sub)
}