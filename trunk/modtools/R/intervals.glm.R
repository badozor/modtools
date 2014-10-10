# take caution with type="tolerance". It is debatable (extension of the lm version for glm)!!! 
# this function is not stabilized !!!
`intervals.glm` <-
function (object, newdata, type = "response", interval = "confidence", 
    method = 1, level = 0.05,...)
{
    if (!inherits(object, "glm")) 
        stop("object 'glm' expected !")
    if (missing(newdata)) 
        newdata <- object$data
    data <- model.matrix(formula(object)[-2], data = newdata)
    if (is.null(object$call$family)) 
        object$family <- gaussian()
    s1 <- summary(object)
    df <- object$df.residual
    pred1 <- predict(object, newdata = newdata, type = "link")
    VCV <- s1$cov.unscaled
    if (method == 1) {
        sigma <- sum((object$weights * object$residuals^2)[object$weights > 
            0])/df
        if (interval == "confidence") 
            sy <- unlist(apply(data, 1, function(x) sqrt(sigma * 
                (t(x) %*% VCV %*% x))))
        else if (interval == "prediction") 
            sy <- unlist(apply(data, 1, function(x) sqrt(sigma * 
                (1 + t(x) %*% VCV %*% x))))
        else stop("non expected 'interval'!")
        lower <- pred1 - sy * qt(1 - level/2, df = df)
        pred1 <- pred1
        upper <- pred1 + sy * qt(1 - level/2, df = df)
        if (type == "response") {
            lower <- object$family$linkinv(lower)
            pred1 <- object$family$linkinv(pred1)
            upper <- object$family$linkinv(upper)
        }
    }
    else if (method == 2) {
        if (interval == "prediction") 
            stop("non expected 'interval'!")
        phi <- s1$dispersion
        sy <- unlist(apply(data, 1, function(x) sqrt((t(x) %*% 
            VCV %*% x))))
        sy <- phi * object$family$variance(pred1) * sy
        lower <- pred1 - sy * qt(1 - level/2, df = df)
        pred1 <- pred1
        upper <- pred1 + sy * qt(1 - level/2, df = df)
    }
    else stop("Non convenient method !")
    res <- as.data.frame(cbind(pred1, lower, upper))
    names(res) <- c("pred", "lower", "upper")
    return(res)
}
