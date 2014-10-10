dispersion <- function(x,...){
	UseMethod("dispersion")
}
dispersion.glm <- function(object,...){
	df.r <- object$df.residual
		if (object$family$family %in% c("poisson", "binomial")) 
			disper <- 1
		else if (df.r > 0) {
			if (any(object$weights == 0)) 
					warning("observations with zero weight not used for calculating dispersion")
			disper <- sum((object$weights * object$residuals^2)[object$weights > 0])/df.r
			}
			else {
			disper <- 	NaN
			}
	return(disper)
}