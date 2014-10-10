mean.rtest <- function(x,y,nrepet = 99){
	xy <- c(x,y)
	group <- c(rep("x",length(x)),rep("y",length(y)))
	obs <- abs(mean(xy[group=="x"])-mean(xy[group=="y"]))
	if (nrepet == 0) 
		return(obs)
	perm <- matrix(0, nrow = nrepet, ncol = 1)
	perm <- apply(perm, 1, function(z){rndgroup <- sample(group); abs(mean(xy[rndgroup=="x"])-mean(xy[rndgroup=="y"]))})
	w <- as.rtest(obs = obs, sim = perm, call = match.call())
	return(w)
}
t.rtest <- function(x,y,nrepet = 99,...){
	xy <- c(x,y)
	group <- c(rep("x",length(x)),rep("y",length(y)))
	obs <- abs(t.test(xy[group=="x"],xy[group=="y"],...)$statistic)
	if (nrepet == 0) 
		return(obs)
	perm <- matrix(0, nrow = nrepet, ncol = 1)
	perm <- apply(perm, 1, function(z){rndgroup <- sample(group); abs(t.test(xy[rndgroup=="x"],xy[rndgroup=="y"],...)$statistic)})
	w <- as.rtest(obs = obs, sim = perm, call = match.call())
	return(w)
}
median.rtest <- function(x,y,nrepet = 99){
	xy <- c(x,y)
	group <- c(rep("x",length(x)),rep("y",length(y)))
	obs <- abs(median(xy[group=="x"])-median(xy[group=="y"]))
	if (nrepet == 0) 
		return(obs)
	perm <- matrix(0, nrow = nrepet, ncol = 1)
	perm <- apply(perm, 1, function(z){rndgroup <- sample(group); abs(median(xy[rndgroup=="x"])-median(xy[rndgroup=="y"]))})
	w <- as.rtest(obs = obs, sim = perm, call = match.call())
	return(w)
	
}
cor.rtest <- function(x,y,nrepet=99,option = "pearson",...){
	if(length(x)!=length(y))
		stop("non convenient dimension!")
	obs <- abs(cor(x,y,...))
	if(nrepet==0)
		return(obs)
	perm <- matrix(0, nrow = nrepet, ncol = 1)
	perm <- apply(perm, 1,function(z,type=option) cor(x,sample(y),method=type))
	w <- as.rtest(obs = abs(obs), sim = abs(perm), call = match.call())
	w$cor <- cor(x,y,...)
	return(w)	
}
diffcor.rtest <- function(x,y,fac,nrepet=99, option = "pearson",...){
	fac <- as.factor(as.character(fac))
	x1 <- x[fac==levels(fac)[1]]
	x2 <- x[fac==levels(fac)[2]]
	y1 <- y[fac==levels(fac)[1]]
	y2 <- y[fac==levels(fac)[2]]
	obs <- abs(cor(x1,y1,method=option)-cor(x2,y2,method=option))
	if (nrepet == 0) 
		return(obs)
	perm <- matrix(0, nrow = nrepet, ncol = 1)
	perm <- apply(perm, 1,function(z,type=option) abs(cor(x1,sample(y1),method=type)-cor(x2,sample(y2),method=type)))
	w <- as.rtest(obs = abs(obs), sim = abs(perm), call = match.call())
	return(w)		
}
