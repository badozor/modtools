fSpecSens2 <-
function(obs,test){
    TP <- sum(obs==1 & test==1)
    FN <- sum(obs==1 & test==0)
    FP <- sum(obs==0 & test==1)
    TN <- sum(obs==0 & test==0)
    sens <- TP/(TP+FN)
    spec <- TN/(TN+FP)
    spec2 <- (spec-1)*(spec-1)
    sens2 <- (1-sens)*(1-sens)
  return(-(spec2+sens2))
}
