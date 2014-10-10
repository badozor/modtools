`sefit.glm` <-
function(object,newdata,interval="confidence",dispersion=TRUE,m=1,...){
  if(!inherits(object,"glm"))
    stop("object 'glm' expected !")
  if(missing(newdata))
    newdata <- object$data
  data <- model.matrix(object$formula[-2],data=newdata)
  if(is.null(object$call$family))
    object$family <- gaussian()
  s1 <- summary(object)
  df <- object$df.residual
  VCV <- s1$cov.unscaled
  # sigma <- s1$dispersion
  if(dispersion)
    sigma <- sum((object$weights * object$residuals^2)[object$weights > 0])/df
  else sigma <- 1
  if(interval=="confidence")
    sy <- unlist(apply(data,1,function(x) sqrt(sigma*(t(x)%*%VCV%*%x))))
  else if(interval=="prediction")
    sy <- unlist(apply(data,1,function(x) sqrt(sigma*(1/m+t(x)%*%VCV%*%x))))
  else stop("non expected 'interval'!")
 return(sy)
}

