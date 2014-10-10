perf.binary <-
function(obs,test){
    TP <- sum(obs==1 & test==1)
    FN <- sum(obs==1 & test==0)
    FP <- sum(obs==0 & test==1)
    TN <- sum(obs==0 & test==0)
    sens <- TP/(TP+FN)
    spec <- TN/(TN+FP)
    prev <- (TP+FN)/(TP+FN+FP+TN)
    pvp <- TP/(TP+FP)
    pvn <- TN/(TN+FN)
    w <- c(sens,spec,pvp,pvn,prev)
    names(w) <- c("sens","spec","pvp","pvn","prev")
    return(w)
}
