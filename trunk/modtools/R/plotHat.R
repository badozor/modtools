plotHat <- function(object,sub="",col="red",col.text="black",label=NULL,thd=TRUE,...){
  if(!inherits(object,"glm"))
    stop("non convenient argument")
  # leverage plot
  n <- length(object$y)
  p <- n-object$df.residual
  hat1 <- influence(object)$hat
  if(is.null(label))
	label <- 1:n
  plot(1:n,hat1,type="h",ylab="leverage (hat)",col=col,panel.first=c(grid()))
  if(thd){
	select1 <- hat1 > 3*p/n
	text(c(1:n)[select1],hat1[select1],label=label[select1],bg="white",col=col.text,adj=c(0.5,-0.1))
  }else text(1:n,hat1,label=label,bg="white",col=col.text,adj=c(0.5,-0.1))
  abline(h=3*p/n,lty=3)
  abline(h=2*p/n,lty=3)
  title(sub)
}