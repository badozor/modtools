fSpecSens <-
function(obs,test){
    TP <- sum(obs==1 & test==1)
    FN <- sum(obs==1 & test==0)
    FP <- sum(obs==0 & test==1)
    TN <- sum(obs==0 & test==0)
    sens <- TP/(TP+FN)
    spec <- TN/(TN+FP)
  return(sens+spec)
}
