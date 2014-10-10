`print.roc` <-
function (x, ...) 
{
    if (!inherits(x, "roc")) 
        stop("object 'roc' expected !")
    print(attr(x, "lim"))
}
