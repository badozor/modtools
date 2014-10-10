modperf.boot <-
function(x,data,cost=costRMSE,R=99,...){
  require(boot)
  if(missing(data))
    data <- x$data
  n <- length(x$y)
  p <- n-x$df.residual
  b1 <- bootvalid(x,data=data,cost=cost,R=R,...)
  b1obs <- b1$t0
  b1bias <- mean(b1$t)-b1$t0
  b1sd <- sd(b1$t)
  res <- c(n,p,b1obs,b1bias,b1sd)
  names(res) <- c("n","p","CostObs","CostBias","CostSd")
 return(res)
}
#
