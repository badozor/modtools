'bootcoef.ci' <-
function(object,type="percent",level=0.05,...){
  if(!inherits(object,"btcoef"))
    stop("non convenient argument")
  R <- object$R
  sim1 <- object$t
  t0 <- object$t0
  tmean <- apply(sim1,2,mean)
  bias1 <- tmean-t0
  sd1 <- apply(sim1,2,sd)
  if(type=="percent"){
    qth <- c(level/2,1-level/2)
    lwr <- 2*t0 - apply(sim1,2,function(x) sort(x)[round(R*qth[1])])
    upr <- 2*t0 - apply(sim1,2,function(x) sort(x)[round(R*qth[2])])
    lower <- apply(cbind(lwr,upr),1,min)
    upper <- apply(cbind(lwr,upr),1,max)
  }
  else if(type=="norm"){
    lwr <- t0 - bias1 - qnorm(1-level/2,0,1)*sd1
    upr <- t0 - bias1 + qnorm(1-level/2,0,1)*sd1
    lower <- apply(cbind(lwr,upr),1,min)
    upper <- apply(cbind(lwr,upr),1,max)
  }
  else
    stop("Non convenient type !")
  res <- data.frame(t0=t0,lower=lower,upper=upper)
  return(res)
}
