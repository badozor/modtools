plotQQres <-
function(object,type="pearson",sub="QQ-norm for residuals",...){
  respearson <- residuals.glm(object,type=type)
  qqnorm(respearson,main=sub,panel.first=c(grid()),...)
  qqline(respearson,col="red",lwd=2)
  abline(0,1,lty=2,lwd=1.5)
}
