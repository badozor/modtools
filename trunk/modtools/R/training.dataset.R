training.dataset <-
function (x, cluster = rep(1, length(x)), ratio = 1/4) 
{
    test <- unlist(tapply(x, cluster, function(j) sample(j, round(length(j) * 
        ratio))))
    res <- data.frame(x = x, test = x %in% test, training = !(x %in% 
        test), cluster = cluster)
    attr(res, "ratio") <- ratio
    attr(res, "N") <- length(x)
    attr(res, "Ntest") <- sum(res$test)
    attr(res, "Ntraining") <- sum(res$training)
    return(res)
}
