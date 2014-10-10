`plot.scorevalid` <-
function (x,mgraph=NULL,...)
{
    if (!inherits(x, "scorevalid"))
        stop("object 'scorevalid' expected !")
    px <- x$px
    px2 <- x$px2
    py <- x$py
    py2 <- x$py2
    p0 <- x$p0
    p1 <- x$p1
    par(mfrow = c(1, 3))
    if(is.null(mgraph))
      par(mfrow = c(1, 3))
    par(mfrow=mgraph)
    # graph 1
    plot(c(0, py/p1 * px), 1 - c(py2 * px2/p0, 0), type = "b",pch=20,cex=1.5,
        xlab = expression(beta), ylab = expression(1 - alpha),
        col = "red",panel.first=c(grid(),segments(0,0,1,1)))
    # graph 2
    plot(c(0, px), c(0, py/p1), type = "b", pch=20,cex=1.5,xlab = "P(S(x)<s)",
        ylab = "P(Y=1|S(x)<s)/P(Y=1)",xlim = c(0, 1), ylim = c(0,1),
            col = "red",panel.first=c(grid(),segments(0,0,1,1),
            segments(c(0, 0), c(0, 1), c(0, 1), c(1, 1))))
    # graph 3
    plot(c(0, px), c(0, py/p1 * px), type = "b", pch=20,cex=1.5,xlab = "P(S(x)<s)",
        ylab = "P(S(x)>s|Y=1)/P(Y=1)", xlim = c(0, 1), ylim = c(0,
            1), col = "red",panel.first=c(grid(),segments(0,0,1,1)))
}

