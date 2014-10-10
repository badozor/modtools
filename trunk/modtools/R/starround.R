add.starsig <- function(x,cutpoints=NULL,symbols=NULL,...){
	if(is.null(cutpoints))
		cutpoints <- c(0, 0.001, 0.01, 0.05, 0.1, 1)
	if(is.null(symbols))
		symbols <- c("***", "**", "*", ".", " ")
	cut(x,breaks=cutpoints,label=symbols)	
}	
add.roundsig <- function(x,digits=3,...){
	ifelse(x < 10^(-digits),paste("<0.",paste(rep(0,digits-1),collapse=""),"1",sep=""),round(x,digits))
}
