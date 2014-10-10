`print.scorevalid` <-
function (x,digits = getOption("digits"), ...)
{
    if (!inherits(x, "scorevalid")) 
        stop("object 'scorevalid' expected !")
    print("in construction!")
    print(x$table.score,digits=digits)
    print(x$table.y,digits=digits)
    print(table(x$y.ordonne),digits=digits)
}
