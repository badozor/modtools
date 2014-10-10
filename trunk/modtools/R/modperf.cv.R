modperf.cv <-
function(x,data,cost=costRMSE,K=10,...){
  require(boot)
  if(!inherits(x,"glm"))
    stop("non convenient argument!")
  if(missing(data))
    data <- x$data
  n <- length(x$y)
  p <- n-x$df.residual
  deltak <- NULL
  for(i in K){
    deltak <- c(deltak,cv.glm(data,x,K=5)$delta)
  }
  res <- c(n,p,deltak)
  names(res) <- c("n","p",as.vector(sapply(K,function(j) paste(c("draw","dadj"),j,sep="."))))
 return(res)
}
