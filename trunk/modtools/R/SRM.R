`SRM` <-
function (y, yhat, h, cost = costRMSE, alpha = 0.05) 
{
    Remp <- cost(y, yhat)
    n <- length(y)
    Comp <- sqrt((h * (log(2 * n/h) + 1) - log(alpha/4))/n)
    return(Remp + Comp)
}
