envelope.bin <-
function(object,data,type="deviance",R=19,tol=1e-8,conf=0.99,...){
  call <- match.call()
  if(!inherits(object,"glm"))
    stop("object 'glm' expected !")
  if(missing(data))
    data <- object$data
  else data <- data
  n <- length(object$y)
  p <- fitted(object)
  alfa <- (1 - conf)/2
  h <- hatvalues(object)
  e <- matrix(0, n, R)
  e1 <- numeric(n)
  e2 <- numeric(n)
  for(i in 1:R){
    ysim <- rbinom(n,1,p)
    data$y <- ysim
    glm0 <- update(object,formula(object),data=data)
    if(type=="likelihood"){
      resDstand <- residuals(glm0,type="deviance")/sqrt(1-h)
      resPstand <- residuals(glm0,type="pearson")/sqrt(1-h)
      resid1 <- sign(resPstand)*sqrt(h*resPstand^2+(1-h)*resDstand^2)
    }else{
      resid1 <- residuals(glm0,type=type)/sqrt(1-h)
      }
    e[,i] <- sort(abs(resid1))
  }
  for (i in 1:n) {
        eo <- sort(e[i, ])
        e1[i] <- quantile(eo, alfa)
        e2[i] <- quantile(eo, 1 - alfa)
    }
    med <- apply(e, 1, median)
    w <- cbind(e1,med,e2)
    return(w)
}
