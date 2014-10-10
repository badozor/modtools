#------------------------------------------------------
# internal validation by bootstrap (see Harrell 2001) 
# 2012-06-08 by pbady
#------------------------------------------------------
bootvalid.coxph <- function (object, data, R = 99, method = "raw",...) 
{
	require(survival)
	if (missing(data)) 
		data <- object$data
	switch(method, raw = 1, corrected = 2, stop("invalid 'method': ",method))
	if (method == "raw") {
		fun1 <- function(data, i, model, ...) {
			mod1 <- update(model, formula(model), data = data[i,])
			data$fit1 <- predict(mod1,newdata=data)
			form1 <- update.formula(mod1$formula,.~ fit1)
			return(survConcordance(form1, data=data[i,])$concordance)
		}
	}
	else {
		fun1 <- function(data, i, model, cost,...) {
			mod1 <- update(model, formula(model), data = data[i,])
			data$fit1 <- predict(mod1,newdata=data)
			form1 <- update.formula(mod1$formula,.~fit1)
			m1 <- survConcordance(form1,data=data[i,])$concordance
			m2 <- survConcordance(form1,data=data)$concordance
			return(c(m1, m2, m1 - m2))
		}
	}
	w <- boot(data, fun1, R = R, model = object,...)
	w$call <- match.call()
	if (method == "corrected") 
		class(w) <- c("bootcorrected", class(w))
	return(w)
}