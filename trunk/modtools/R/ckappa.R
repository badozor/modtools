ckappa <-
function(x){
  if(ncol(x)!=nrow(x))
    stop("non covenient dimension !")
  N <- sum(x)
  sxii <- sum(diag(x))
  sxip <- apply(x,2,sum)
  sxpi <- apply(x,1,sum)
  k <- (N*sxii-sum(sxip*sxpi))/(N*N-sum(sxip*sxpi))
  return(k)
}
