'bootcoef' <-
function(object,data,R=99,...){
  if(missing(data))
    data <- object$data
  fun1 <- function(data,i,model){
    mod1 <- update(model,formula=formula(model),data=data[i,])
    return(coef(mod1))
    }
  w <- boot(data,fun1,R=R,model=object)
  w$call <- match.call()
  class(w) <- c("btcoef",class(w))
  return(w)
}
