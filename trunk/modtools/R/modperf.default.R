modperf.default <-
function (x, data, cost = costRMSE, R = 99, ...) 
{
    if (!inherits(x, "glm")) 
        stop("non convenient argument!")
    if (missing(data)) 
        data <- x$data
    b1 <- bootvalid(x, data = data, cost = cost, R = R, ...)
    b1obs <- b1$t0
    b1bias <- mean(b1$t) - b1$t0
    b1sd <- sd(b1$t)
    n <- length(x$y)
    p <- n - x$df.residual
    aic1 <- AIC(x, k = 2)
    bic1 <- AIC(x, k = log(n))
    aicc1 <- aic1 + 2*p*(p+1)/(n-p-1)
    R2 <- summary(lm(x$y ~ x$fitted))$r.squared
    res2 <- rstandard(x)^2
    CovPat <- sum(as.numeric(res2 > 4))/length(res2)
    delta5 <- cv.glm(data, x, K = 5)$delta
    delta10 <- cv.glm(data, x, K = 10)$delta
    res <- c(n, p, aic1, aicc1, bic1, R2, CovPat, b1obs, b1bias, 
        b1sd, delta5, delta10)
    names(res) <- c("n", "p", "AIC", "AICc", "BIC", "R2", "CovPat", 
        "CostObs", "CostBias", "CostSd", "d5.raw", "d5.adj", 
        "d10.raw", "d10.adj")
    return(res)
}
