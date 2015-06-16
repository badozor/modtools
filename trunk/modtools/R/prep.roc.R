prep.roc <- function (obs, pred, method = "max", subset, nbval = NULL,cutoff=NULL,...) 
{
    if (length(obs) != length(pred)) 
        stop("non convenient dimension !")
    if (!missing(subset)) {
        obs <- obs[subset]
        pred <- obs[subset]
    }
    if (!is.null(nbval)) {
        cut1 <- ppoints(nbval)
    }
    else {
        cut1 <- c(0, sort(pred), 1)
    }
    res <- NULL
    for (i in 1:length(cut1)) {
        pred01 <- as.factor(ifelse(pred >= cut1[i], 1, 0))
        res <- rbind(res, c(cut1[i], perf.binary(obs, pred01)))
    }
    res <- as.data.frame(res)
    names(res) <- c("cut", "sens", "spec", "pvp", "pvn", "prev")
    if (method == "estimate") {
        opcut <- optimCut(obs, pred, ...)
        opcut <- opcut$maximum
    }
    else if (method == "max") {
        xx <- res$sens + res$spec
        opcut <- res[xx == max(xx), ]$cut[1]
    }
    else if (method == "max2") {
        spec2 <- (res$spec - 1) * (res$spec - 1)
        sens2 <- (1 - res$sens) * (1 - res$sens)
        xx <- (-(spec2 + sens2))
        opcut <- res[xx == max(xx), ]$cut[1]
    }else if(method=="equal"){
        xx <- 1-abs(res$sens - res$spec)
        opcut <- res[xx == max(xx), ]$cut[1]
    }else if(method=="manual"){ 
      optcut <- cutoff      
    }else stop("Non convenient method !")
    pred <- ifelse(pred >= opcut, 1, 0)
    confmat <- matrix(0, 2, 2)
    confmat[1, 1] <- sum(obs == 1 & pred == 1)
    confmat[2, 1] <- sum(obs == 1 & pred == 0)
    confmat[1, 2] <- sum(obs == 0 & pred == 1)
    confmat[2, 2] <- sum(obs == 0 & pred == 0)
    confmat <- as.data.frame(confmat)
    names(confmat) <- row.names(confmat) <- c("1", "0")
    attr(res, "table") <- confmat
    w <- c(opcut, perf.binary(obs, pred))
    names(w) <- c("cut", "sens", "spec", "pvp", "pvn", "prev")
    attr(res, "lim") <- as.data.frame(t(w))
    class(res) <- c("roc", "data.frame")
    return(res)
}