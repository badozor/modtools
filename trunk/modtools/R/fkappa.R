fkappa <-
function(obs,test){
    TP <- sum(obs==1 & test==1)
    FN <- sum(obs==1 & test==0)
    FP <- sum(obs==0 & test==1)
    TN <- sum(obs==0 & test==0)
  x <- matrix(c(TP,FN,FP,TN),2,2)
  N <- sum(x)
  sxii <- sum(diag(x))
  sxip <- apply(x,2,sum)
  sxpi <- apply(x,1,sum)
  k <- (N*sxii-sum(sxip*sxpi))/(N*N-sum(sxip*sxpi))
  return(k)
}
