plotObsExpCat <-
function(object,sub="Expected vs observed values",horizontal=TRUE,...){
  if(!inherits(object,"glm"))
    stop("non convenient argument")
  mu <-  predict(object,type="response")
  lim1 <- c(min(c(object$y,object$fitted)),max(c(object$y,object$fitted)))
  boxplot(mu~object$y,ylab="expected values (mu)",xlab="observed values",ylim=lim1,
    horizontal=horizontal,col ="gray",pch=20,cex=1.5,...)
  title(sub)
}
