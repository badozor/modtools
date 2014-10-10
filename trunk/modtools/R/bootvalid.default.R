`bootvalid.default` <-
function(object,data,cost=costMSE,R=99,method="raw",...){
  if(missing(data))
    data <- object$data
  switch(method, raw = 1,corrected = 2, stop("invalid 'method': ", method))
  if(method=="raw"){
    fun1 <- function(data,i,model,cost,...){
      mod1 <- update(model,formula=formula(model),data=data[i,])
      return(cost(mod1$y,mod1$fitted,...))
      }
  }else{
    fun1 <- function(data,i,model,cost,...){
      mod1 <- update(model,formula=formula(model),data=data[i,])
      mu <- predict(mod1,newdata=data,type="response")
      m1 <- cost(mod1$y,mod1$fitted,...)
      m2 <- cost(model$y,mu,...)
      return(c(m1,m2,m1-m2))
      }
  }
  w <- boot(data,fun1,R=R,model=object,cost=cost,...)
  w$call <- match.call()
  if(method=="corrected")
    class(w) <- c("bootcorrected",class(w))
  return(w)
}
