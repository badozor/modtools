`anscresid` <-
function (object, ...) 
{
    if (!inherits(object, "glm")) 
        stop("object 'glm' expected !")
    y <- object$y
    p <- object$fitted
    p <- object$fitted
    m <- object$prior.weights
    bt <- function(s, ...) {
        tu <- function(x) (x^(-1/3)) * ((1 - x)^(-1/3))
        res <- unlist(sapply(s, function(x) ifelse(x == 0, 0, 
            integrate(tu, 0, x))))
        return(res)
    }
    if (family(object)$family == "binomial") {
        q <- 1 - p
        res <- sqrt(m) * (bt(y) - bt(p)) * (p * q)^(-1/6)
    }
    else if (family(object)$family == "gaussian") 
        res <- y - p
    else if (family(object)$family == "inverse.gaussian") 
        res <- (log(y) - log(p))/(p^0.5)
    else if (family(object)$family == "Gamma") 
        res <- 3 * ((y/p)^(1/3) - 1)
    else if (family(object)$family == "poisson") 
        res <- (3/2) * ((y^(2/3)) * p^(-1/6) - p^0.5)
    else stop("non convenient family !")
    return(res)
}
