optimCut <-
function(obs,pred,func = fSpecSens2,...){
  f1 <- function(x,obs,pred,func){
    pred01 <- ifelse(pred >= x,1,0)
    func(obs,pred01)
  }
  res <- optimize(f1,c(0,1),obs=obs,pred=pred,func=func,maximum=TRUE,...)
  return(res)
}
