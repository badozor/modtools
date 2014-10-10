plotResDens <-
function(object,type="pearson",sub="Residuals histogram",nclass=13,...){
  respearson <- residuals.glm(object,type=type)
  hist(respearson,nclass=nclass,proba=TRUE,main=sub,col="gray",xlab="residuals")
  curve(dnorm(x,mean=mean(respearson),sd=sqrt(var(respearson))),add=TRUE,lwd=2,...)
  lines(density(respearson),col="red",lwd=2)
  abline(v=0,lty=2,lwd=1.5)
}
