plotObsExp <-
function(object,sub="Expected vs observed values",...){
  if(!inherits(object,"glm"))
    stop("non convenient argument")
  mu <-  predict(object,type="response")
  lim1 <- c(min(c(object$y,object$fitted)),max(c(object$y,object$fitted)))
  plot(mu,object$y,xlab="expected values (mu)",ylab="observed values",xlim=lim1,ylim=lim1,panel.first=c(grid()),pch=20,cex=1.5)
  abline(0,1,lty=2)
  abline(lm(object$y ~ object$fitted),col="red",lty=2, lwd=2)
  lines(lowess(object$y ~ object$fitted),col="green",lty=2, lwd=2)
  title(sub)
}
