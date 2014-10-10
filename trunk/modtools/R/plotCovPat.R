plotCovPat <- function(object,sub="DX2 and Dbeta",plot=TRUE,...){
  if(!inherits(object,"glm"))
    stop("non convenient argument")
  infl1 <- influence(object)
  hat1 <- infl1$hat
  # covariate patterns
  mu <-  predict(object,type="response")
  # prendre les standardizés
  rstd <- rstandard(object)
  dbeta <- hat1*(rstd*rstd)/(1-hat1)
  if(!plot){
    return(data.frame(mu=mu,rstd=rstd,hat=hat1,dX2=rstd^2,dbeta=dbeta))
  }else{
    plot(mu,rstd^2,type="h",xlab="fitted",ylab=expression(Delta(X[i]^{2})))
    points(mu,rstd^2,pch=20,cex=1.5)
    symbols(mu,rstd^2,dbeta,inch=0.15,bg="gray",cex=1.5,add=TRUE)
    abline(h=4,col="red",lwd=2,lty=2)
    title(sub)
  }
}