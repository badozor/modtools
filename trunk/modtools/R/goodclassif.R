`goodclassif` <-
function (x) 
{
    if (ncol(x) != nrow(x)) 
        stop("non covenient dimension !")
    N <- sum(x)
    sxii <- sum(diag(x))
    return(sxii/N)
}
