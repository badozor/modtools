sim.intervals <-
function(object,newdata,type="response",interval="confidence",qth=c(0.025,0.975),R=100,m=1,sigma){
  if(!inherits(object,"glm"))
    stop("object 'glm' expected !")
  if(missing(newdata))
    newdata <- object$data
  data <- model.matrix(object$formula[-2],data=newdata)
  if(is.null(object$call$family))
    object$family <- gaussian()
  s1 <- summary(object)
  df <- object$df.residual
  pred1 <- predict(object,newdata=newdata,type="link")
  VCV <- s1$cov.unscaled
  #sigma <- s1$dispersion
  if(missing(sigma))
    sigma <- sum((object$weights * object$residuals^2)[object$weights > 0])/df
  if(interval=="confidence")
      sy <- unlist(apply(data,1,function(x) sqrt(sigma*(t(x)%*%VCV%*%x))))
    else if(interval=="prediction")
      sy <- unlist(apply(data,1,function(x) sqrt(sigma*(1/m+t(x)%*%VCV%*%x))))
    else stop("non expected 'interval'!")
    pred1 <- pred1
    tabsim1 <- t(apply(cbind(pred1,sy),1,function(x) rnorm(R,x["pred1"],x["sy"])))
    qthsim1 <- t(apply(tabsim1,1,function(x) quantile(x,qth)))
    upr <-  qthsim1[,2]
    lwr <-  qthsim1[,1]
    if(type=="response"){
      lwr <- object$family$linkinv(lwr)
      pred1 <- object$family$linkinv(pred1)
      upr <- object$family$linkinv(upr)
    }
  lower <- apply(cbind(lwr,upr),1,min)
  upper <- apply(cbind(lwr,upr),1,max)
  res <- as.data.frame(cbind(pred1,sy,lower,upper))
  names(res) <- c("pred","se","lower","upper")
  return(res)
}
