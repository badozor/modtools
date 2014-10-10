### general
LRtest <-function (object, ...){
	UseMethod("LRtest")
}
delete.intercept <- function (mm){
	saveattr <- attributes(mm)
	intercept <- which(saveattr$assign == 0)
	if (!length(intercept))
		return(mm)
	mm <- mm[, -intercept, drop = FALSE]
	saveattr$dim <- dim(mm)
	saveattr$dimnames <- dimnames(mm)
	saveattr$assign <- saveattr$assign[-intercept]
	attributes(mm) <- saveattr
	mm
}
delete.space <- function(x)
	gsub(" ","",x)
varnames <-function (object, ...){
	UseMethod("varnames")
}
varnames.default <- function(object, ...){
	form <- delete.space(as.character(formula(object)[[3]]))
	form <- unlist(strsplit(form,"[+]"))
	form <- form[nchar(form) > 0]
	return(form)
}
permtest <-function (object, ...){
	UseMethod("permtest")
}
permtest2df <- function(x){
	if (!inherits(x, "permtest")) 
		stop("Non convenient data")
	obs <- unlist(lapply(x,function(z) z$obs))
	rep <- unlist(lapply(x,function(z) z$rep))
	pvalue <- unlist(lapply(x,function(z) z$pvalue))
	data.frame(obs,rep,pvalue)
}
print.permtest <- function (x, ...){
	if (!inherits(x, "permtest")) 
		stop("Non convenient data")
	cat("Monte-Carlo test\n")
	cat("Call: ")
	print(attributes(x)$call)
	print(permtest2df(x))
}
### negative binomial
LRtest.negbin <- function(mod0,mod1,...){
	df <- mod0$df.residual-mod1$df.residual
	X2 <- -(mod0$twologlik-mod1$twologlik)
	pvalue <- 1-pchisq(X2,df)
	return(list(df=df,X2=X2,p.value=pvalue))
}
permtest.negbin <- function(object,variable,data=NULL,B
				= 999,...){
	if (!inherits(object, "negbin"))
		stop("Object of type 'negbin' expected")
	if(is.null(data))
		data <- object$data
	if(is.null(data) & is.null(object$data))
		stop("the object 'data' is missing!")
	modvariable <- varnames(object)
	modvariable <- unlist(sapply(variable,function(x)
						grep(x,modvariable,value=TRUE)))
	res <- list()
	for(k in 1:length(modvariable)){
		auxi <- modvariable[k]
		form0 <- as.formula(paste(". ~ .","-",auxi))
		mod0 <- update(object,formula=form0,data=data)
		obs <- -(mod0$twologlik-object$twologlik)
		sim <- rep(NA,B)
		tmp <- data
		for(i in 1:B){
			tmp[,names(modvariable)[k]] <-
					sample(data[,names(modvariable)[k]])
			modx <- update(object,data=tmp)
			sim[i] <- -(mod0$twologlik-modx$twologlik)
		}
		res[[k]] <- as.rtest(sim,obs)
	}
	names(res) <- variable
	attr(res,"k")<- length(variable)
	attr(res,"variable") <- variable
	attr(res,"call") <- match.call()
	class(res) <- "permtest"
	return(res)
}
### lmekin (from kinship => deprecated!)
LRtest.lmekin <- function(mod0,mod1,...){ 
	df <- mod0$df.residual-mod1$df.residual
	X2 <- -2*(mod0$loglik-mod1$loglik)
	pvalue <- 1-pchisq(X2,df)
	return(list(df=df,X2=X2,p.value=pvalue))
}
formula.lmekin <- function (x, ...) 
	eval(x$call$fixed)
update.lmekin <- function (object, fixed., ..., evaluate = TRUE){
	call <- object$call
	if (is.null(call)) 
		stop("need an object with call component")
	extras <- match.call(expand.dots = FALSE)$...
	if (!missing(fixed.)) 
		call$fixed <- update.formula(formula(object), fixed.)
	if (length(extras) > 0) {
		existing <- !is.na(match(names(extras), names(call)))
		for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
		if (any(!existing)) {
			call <- c(as.list(call), extras[!existing])
			call <- as.call(call)
		}
	}
	if (evaluate) 
		eval(call, parent.frame())
	else call
}
permtest.lmekin <- function(object,variable,data=NULL,kmat=NULL,B = 999,...){
	if (!inherits(object, "lmekin")) 
		stop("Object of type 'lmekin' expected")
	if(is.null(data))
		data <- object$data
	if(is.null(kmat))
		kmat <- object$kmat
	if(is.null(data) & is.null(object$data))
		stop("the object 'data' is missing!")
	if(is.null(kmat) & is.null(object$kmat))
		stop("the object 'kmat' is missing!")
	modvariable <- varnames(object)
	modvariable <- unlist(sapply(variable,function(x) grep(x,modvariable,value=TRUE)))
	res <- list()
	for(k in 1:length(modvariable)){
		auxi <- modvariable[k]
		form0 <- as.formula(paste(". ~ .","-",auxi))
		mod0 <- update(object,fixed.=form0,data=data,varlist=kmat)
		obs <- -2*(mod0$loglik-object$loglik)
		sim <- rep(NA,B)
		tmp <- data
		for(i in 1:B){
			tmp[,names(modvariable)[k]] <- sample(data[,names(modvariable)[k]])
			modx <- update(object,data=tmp,varlist=varlist)
			sim[i] <- -2*(mod0$loglik-modx$loglik)
		}
		res[[k]] <- as.rtest(sim,obs)
	}
	names(res) <- variable
	attr(res,"k")<- length(variable)
	attr(res,"variable") <- variable
	attr(res,"call") <- match.call()
	class(res) <- "permtest"
	return(res)
}
# get variance for lmekin object
getVariance <- function (x, ...){
	UseMethod("getVariance")
}
getVariance.lmekin <- function(x,...){
	rcoef <- x$theta
	temp <- matrix(c(sqrt(rcoef), rcoef/sum(rcoef)), nrow = 1,byrow = T)
	dimnames(temp)[[2]] <- paste(names(rcoef),c("SE","SE","Per","Per"),sep="_")
	temp
}

