list2df <- function(object,rnames=NULL,...){
	if(is.null(rnames))
		rnoms <- sort(unique(unlist(sapply(1:length(object),function(x) names(object[[x]])))))
	else rnoms <- rnames
	cnoms <- names(object)
	res <- matrix(0,nrow=length(rnoms),ncol=length(cnoms))
	res <- as.data.frame(res)
	row.names(res) <- rnoms
	names(res) <- cnoms
	for(i in 1:length(object))
		res[names(object[[i]]),names(object)[i]] <- object[[i]]
	return(res)
}
#------- table adjustment -------
rescale.names <- function(tab,ref,method ="row.row"){
	if(!inherits(tab,"data.frame"))
		stop("non convenient argument")
	if(!inherits(ref,"data.frame"))
		stop("non convenient argument")
	if(method =="row.row"){
		vec <- intersect(row.names(ref),row.names(tab))
		return(tab[vec,])
	}
	else if(method =="row.col"){
		vec <- intersect(names(ref),row.names(tab))
		return(tab[vec,])
	}
	else if(method =="col.col"){
		vec <- intersect(names(ref),names(tab))
		return(tab[,vec])
	}
	else if(method =="col.row"){
		vec <- intersect(row.names(ref),names(tab))
		return(tab[,vec])
	}
	else stop("non convenient method!")
}
#------- binary transformation -------
scale01 <- function (x){
	xx <- as.matrix(x)
	x01 <- as.numeric(xx > 0)
	dim(x01) <- dim(xx)
	x01 <- as.data.frame(x01)
	names(x01) <- names(x)
	row.names(x01) <- row.names(x)
	return(x01)
}
#-------------------------------------------------
# author: Maxime Logez <maxime.logez@cemagref.fr
# 09/17/08 18:06:19
scaleby <- function(x,fac,center=TRUE,scale=TRUE){
	w <- lapply(split(x,fac),scale,center,scale)
	means <- sapply(w,attr,which="scaled:center")
	sds <- sapply(w,attr,which="scaled:scale")
	x <- do.call("rbind",w)
	attributes(x)$center <- means
	attributes(x)$scale <- sds
	attributes(x)$fac <- as.character(fac)
	return(x)
}
#------- functions "Replace" -------------
# argument class: data.frame
Replace <- function (x,old,new){
	UseMethod("Replace")
}
Replace.data.frame <- function(x,old,new){
	if(!inherits(x,"data.frame"))
		stop("non convenient argument")
	f1 <- function(x,old,new){
		if(is.na(old)){
			if(is.factor(x)){
				x <- as.character(x)
				x[is.na(x)] <- rep(new,length(x[is.na(x)]))
				x <- as.factor(x)
			}
			else
				x[is.na(x)] <- rep(new,length(x[is.na(x)]))
			return(x)
		}
		else {
			if(is.factor(x)){
				x <- as.character(x)
				x[x==old & !is.na(x)] <- rep(new,length(x[x==old & !is.na(x)]))
				x <- as.factor(x)
			}
			else
				x[x==old & !is.na(x)] <- rep(new,length(x[x==old & !is.na(x)]))
			return(x)
		}
	}
	tab <- data.frame(lapply(x,function(y) f1(y,old,new)))
	names(tab) <- names(x)
	row.names(tab) <- row.names(x)
	return(tab)
}
# argument class: factor
Replace.factor <- function(x,old, news){
	if(!inherits(x,"factor"))
		stop("non convenient argument")
	x <- as.character(x)
	if(is.na(old)){
		ted <- is.na(x)
		nx <- length(x[ted])
		x[ted] <- rep(news,nx)
		x <- as.factor(x)
	}
	else{
		ted <- x==old & !is.na(x)
		nx <- length(x[ted])
		x[ted] <- rep(news,nx)
		x <- as.factor(x)
	}
	return(x)
}
# argument class: numeric or character
Replace.default <- function(x,old, news){
	if(!any(!inherits(x,"numeric"),!inherits(x,"character")))
		stop("non convenient argument")
	if(is.na(old)){
		ted <- is.na(x)
		nx <- length(x[ted])
		x[ted] <- rep(news,nx)
	}
	else{
		ted <- x==old & !is.na(x)
		nx <- length(x[ted])
		x[ted] <- rep(news,nx)
	}
	return(x)
}
deleteSpace <- function(x,...){ 
	gsub("(^\\s+|\\s+$)","",x,perl=TRUE,...)
}
REbracketout <-	function (x, replaceby = "", ...){
	x <- unlist(sapply(x, function(z) gsub("([(])|([)])", replaceby,z)))
	return(x)
}
REspaceout <-function (x, replaceby = "", ...){
	x <- unlist(sapply(x, function(z) gsub(" ", replaceby, z)))
	return(x)
}
REzeroout <-function (x, replaceby = "", ...){
	x <- unlist(sapply(x, function(z) gsub("?([.][0])", replaceby,z)))
	return(x)
}
#-------------------------------
# frequency table
#-------------------------------
freq <- function(x,option="row",na.rm=FALSE){
# option=c("row","col","all")
	if(option=="row"){
		x <- apply(x,1,function(x)x/sum(x,na.rm=na.rm))
		x <- as.data.frame(t(x))
	}
	else if(option=="col"){
		x <- apply(x,2,function(x)x/sum(x,na.rm=na.rm))
	}
	else if(option=="all"){
		x <- x/sum(x,na.rm=na.rm)
	}
	else stop("non convient option !")
	return(x)
}
xtabs2df <- function (x){
	if (!inherits(x, "xtabs"))
		stop("non convenient argument")
	x <- as.data.frame(unclass(x))
	return(x)
}
table2df <- function (x){
	if (!inherits(x, "table"))
		stop("non convenient argument")
	x <- as.data.frame(unclass(x))
	return(x)
}
acm.util <- function(x) {
	rnames <- names(x)
	cl <- as.factor(x)
	n <- length(cl)
	cl <- as.factor(cl)
	x <- matrix(0, n, length(levels(cl)))
	x[(1:n) + n * (unclass(cl) - 1)] <- 1
	dimnames(x) <- list(rnames,levels(cl))
	return(x)
}
#-------------------------------------
#    le 29/05/2002 P.BADY
#-------------------------------------
histnorm <- function(vec,x1=-5,x2=5,...)
{
	hist(vec,proba=T,col="grey",...)
	xo <- seq(x1,x2,le=1000)
	yo <- dnorm(xo,mean(vec),sqrt(var(vec)))
	lines(xo,yo,col="blue")
}
histchisq <- function(vec,x1=0,x2=40,df=1,...)
{
	hist(vec,proba=T,col="grey",...)
	xo <- seq(x1,x2,le=1000)
	lines(xo,dchisq(xo,df=df),col="blue")
}
histclass <- function(y,fac,nclass=8,freq=F,mfrow=n2mfrow(nlevels(fac)),...) {
	if (!is.factor(fac)) stop ("factor expected")
	if (!is.vector(y)) stop ("vector expected")
	if (length(y)!=length(fac)) stop("Non equal length")
	opar <- par(mar=par("mar"),mfrow=par("mfrow"))
	on.exit (par(opar))
	nf <- nlevels(fac)
	par(mfrow=mfrow)
	par(mar=c(2.1,2.1,1.1,0.1))
	br0 <- hist(y,nclass=nclass,plot=F)$breaks
	lev <- levels(fac)
	for (i in 1:nf) {
		z <- y[fac==lev[i]]
		hist.default(z,br=br0,freq=freq,main=as.character(lev[i]))
	}
}
#------------------------------------
# calcul d'une combinaison en prob
# 02/27/07 15:34:25
#------------------------------------
Combi <- function(n,m) factorial(m)/(factorial(n)*factorial(m-n))

#--------------------------------------------
# author : J.Lobry
# http://pbil.univ-lyon1.fr/R/querep/qro.pdf
# Logiciel R version 2.4.1 (2006-12-18)
# Compile le 2007-02-07
#--------------------------------------------
polycurve <- function(x, y, base.y = min(y), ...) {
	polygon(x = c(min(x), x, max(x)), y = c(base.y, y, base.y),...)
}
