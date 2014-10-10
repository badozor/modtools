# bootvalid function for lda
bootvalid.lda <- function(object,data,cost="costKappa2",R=99,method="raw",...){
	if(missing(data))
		data <- model.frame(object)
	switch(method, raw = 1,corrected = 2, stop("invalid 'method': ", method))
	if(is.element(cost,c("costKappa2","costDA"))){
		if(method=="raw"){
			fun1 <- function(data,i,model,cost,...){
				mod1 <- update(model,formula=formula(model),subset=NULL,data=data[i,])
				class1 <- predict(mod1,newdata=data[i,])$class
				obs1 <- data[i,as.character(formula(model)[[2]])]
				return(cost(obs1,class1))
			}
		}else{
			fun1 <- function(data,i,model,cost,...){
				mod1 <- update(model,formula=formula(model),subset=NULL,data=data[i,])
				class1 <- predict(mod1,newdata=data[i,])$class
				obs1 <- data[i,as.character(formula(model)[[2]])]
				class2 <- predict(mod1,newdata=data)$class
				obs2 <- data[,as.character(formula(model)[[2]])]
				m1 <- cost(obs1,class1)
				m2 <- cost(obs2,class2)
				return(c(m1,m2,m1-m2))
			}
		}
	}else if(is.element(cost,c("costEntropy","costGini"))){
		if(method=="raw"){
			fun1 <- function(data,i,model,cost,...){
				mod1 <- update(model,formula=formula(model),subset=NULL,data=data[i,])
				return(cost(predict(mod1,newdata=data[i,])$posterior))
			}
		}else{
			fun1 <- function(data,i,model,cost,...){
				mod1 <- update(model,formula=formula(model),subset=NULL,data=data[i,])
				m1 <- cost(predict(mod1,newdata=data[i,])$posterior)
				m2 <- cost(predict(mod1,newdata=data)$posterior)
				return(c(m1,m2,m1-m2))
			}
		}	
	} else stop("non convenient cost function!")
	cost <- eval(parse(text=cost))
	w <- boot(data,fun1,R=R,model=object,cost=cost,...)
	w$call <- match.call()
	if(method=="corrected")
		class(w) <- c("bootcorrected",class(w))
	return(w)
}
