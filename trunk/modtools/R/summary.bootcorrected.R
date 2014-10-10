summary.bootcorrected <-
function(object,display=TRUE,...){
  if(!inherits(object,"bootcorrected"))
    stop("non convenient argument")
  w <- c(object$t0[1],mean(object$t[,1]),mean(object$t[,2]),
    mean(object$t[,3]),object$t0[1]-mean(object$t[,3]))
  names(w) <- c("initial","training","test","optimism","corrected.value")
  if(!display){
    return(w)
  }else{
    print(object,...)
    cat("\n\nValidation values:\n")
    print(w)
  }
}
