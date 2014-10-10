plotEtaRes <-
function(object,type="pearson",sub="Residual structure"){
  # fitted and residuals
  eta <- predict(object,type="link")
  respearson <- residuals.glm(object,type=type)
  plot(eta,respearson,panel.first=c(grid()),xlab="fitted values (eta)",ylab="Residuals",pch=20,cex=1.5)
  abline(h=0,lty=2,lwd=2)
  abline(h=c(-1.64,1.64),lty=2)
  lines(lowess(respearson~eta),col="red",lwd=2)
  title(sub)
}
