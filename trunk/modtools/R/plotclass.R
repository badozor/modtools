plotclass  <-
function(x,fac,xlim=c(min(x),max(x)),bw=diff(xlim)/20,from=xlim[1],to=xlim[2],col="black",
  cols=2:(length(levels(fac))+1),main = "Density representation",nd = 512,
    addmean=TRUE,addrug=TRUE,add.lgd=TRUE,na.rm=FALSE,pos.lgd,...){
  if (!is.numeric(x))
    stop("non covenient argument")
  if (!is.factor(fac))
    stop("non covenient argument")
  if(length(x)!=length(fac))
    stop("non covenient dimension!")
  n <- length(x)
  p <- nlevels(fac)
  dx <- matrix(0,nr=nd,nc=p)
  dy <- matrix(0,nr=nd,nc=p)
  np <- unlist(tapply(rep(1,n),fac,sum))
  names(np) <- levels(fac)
  for( i in 1:p){
    w <- density(x[fac==levels(fac)[i]],bw=bw,from=from,to=to,n=nd,na.rm=na.rm)
    dx[,i] <-  w$x
    dy[,i] <-  w$y
    }
  dtot <- density(x,bw=bw,from=from,to=to,n=nd,na.rm=na.rm)
  y0 <- max(dtot$y)
  plot(dtot,main=main,lwd=2,col=col,...)
  for(i in 1:p){
    lines(dx[,i],dy[,i]*np[names(np)==levels(fac)[i]]/n,col=cols[i],lwd=2,lty=2)
    z <- x[fac==levels(fac)[i]]
    if(addmean){
      mz <- mean(z,na.rm=na.rm)
      lines(c(mz, mz), c(y0*2/3, 0),col=cols[i],lwd=2)
      points(mz, y0*2/3, pch = 18, cex = 2,col=cols[i])
    }
    if(addrug)
      rug(z,col=cols[i],lwd=2)
  }
  if(add.lgd){
    if(missing(pos.lgd)) pos.lgd <-  c(xlim[1],y0*0.9)
    legend(pos.lgd[1],pos.lgd[2],legend=c("All",levels(fac)),col=c(col,cols),lty=c(1,2,2),lwd=2,bg="white")
  }
}
