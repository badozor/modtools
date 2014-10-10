plotBoot <-
function(object,cost=costBIN,R=200,nclass=13,
  sub=paste("Bootstrap (R=",R,")",sep=""),plot=TRUE,...){
  if(!inherits(object,"glm"))
    stop("non convenient argument")
  w <- bootvalid(object,cost=cost,R=R,...)
  if(!plot){
    return(w)
  }else{
    histsim(sim=w$t,obs=w$t0,main=sub,nclass=nclass)
  }
}
