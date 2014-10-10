fgoodclassif <-
function(obs,test){
    TP <- sum(obs==1 & test==1)
    TN <- sum(obs==0 & test==0)
  return((TP+TN)/length(obs))
}
