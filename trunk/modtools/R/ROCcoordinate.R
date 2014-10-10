ROCcoordinate <-
function(sens,spec,type){
  if(length(sens)!=length(spec))
    stop("non convenient dimension!")
  yy <- c(1,sens,0)
  xx <- c(1,1 - spec,0)
  if (type == "curve") {
      #ord1 <- order(xx)
      #xx <- xx[ord1]
      #yy <- yy[ord1]
  }else if (type == "square1") {
      #ord1 <- order(xx)
      xx <- xx #[ord1]
      yy <- yy #[ord1]
      vec <- NULL
      for (i in 1:length(xx)) vec <- c(vec, i, i)
      yy <- yy[vec[-length(vec)]]
      xx <- xx[vec[-1]]
  }else if (type == "square2"){
      #ord1 <- order(xx)
      xx <- xx #[ord1]
      yy <- yy #[ord1]
      vec <- NULL
      for (i in 1:length(xx)) vec <- c(vec, i, i)
      xx <- xx[vec[-length(vec)]]
      yy <- yy[vec[-1]]
  }else stop("Non convenient method !")
  res <- data.frame(xx,yy)
  return(res)
}
