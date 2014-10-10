plotHalfnorm <-
function(object,sub="Half-normal plot",type="deviance",env=TRUE,...){
  if(!inherits(object,"glm"))
    stop("object 'glm' expected !")
  h <-  hatvalues(object)
  if(type=="likelihood"){
      resDstand <- residuals(object,type="deviance")/sqrt(1-h)
      resPstand <- residuals(object,type="pearson")/sqrt(1-h)
      resid1 <- sign(resPstand)*sqrt(h*resPstand^2+(1-h)*resDstand^2)
    }else{
      resid1 <- residuals(object,type=type)/sqrt(1-h)
    }
  resid1 <- abs(resid1)
  qqpl <- qqplot(qnorm((ppoints(resid1)+1)/2),resid1,plot=FALSE)
  if(env){
    simenv <- envelope.bin(object,type=type,...)
  }
  plot(qqpl,ylim=c(0,ceiling(max(c(simenv[,2],resid1)))),pch=20,cex=1.5,
    xlab="half-normal quantiles",ylab=paste(type,"residuals"),panel.first=c(grid()))
  abline(0,1,lty=2,col="red",lwd=1.5)
  if(env){
    lines(qqpl$x,simenv[,2],lty=1,col=grey(0.5))
    lines(qqpl$x,simenv[,1],lty=2,col=grey(0.5))
    lines(qqpl$x,simenv[,3],lty=2,col=grey(0.5))
  }
  title(sub)
}
