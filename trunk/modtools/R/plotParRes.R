plotParRes <-
function(object,mgraph=NULL,...){
  require(faraway)
  prplot <- function (g, i,...){
    xl <- attributes(g$terms)$term.labels[i]
    yl <- paste("beta*", xl, "+res", sep = "")
    x <- model.matrix(g)[, i + 1]
    y <- g$coeff[i + 1] * x + g$res
    plot(x,y, xlab = xl, ylab = yl,...)
    lines(lowess(y~x),col="red")
    abline(0, g$coeff[i + 1],col=gray(0.5))
    invisible()
  }
  k <- length(attributes(object$terms)$term.labels)
  if(is.null(mgraph))
    mgraph<- n2mfrow(k)
  par(mfrow=mgraph)#, mar=c(4,4,2,2))
  for(i in 1:k)
    prplot(object,i,panel.first=c(grid()),pch=20,cex=1.5,...)
}
