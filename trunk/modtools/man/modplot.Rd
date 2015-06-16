\name{modplot}
\alias{plotBoot}
\alias{plotCovPat}
\alias{plotEtaRes}
\alias{plotHalfnorm}
\alias{plotLeverage}
\alias{plotObsExp}
\alias{plotObsExpCat}
\alias{plotParRes}
\alias{plotQQres}
\alias{plotResDens}
\alias{envelope.bin}
\title{Plot Diagnostics for glm and lm Objects}
\description{
Ten plots are currently available for model assessment:
Observed vs Expected values, Normal QQ-plot of residuals,etc...
}
\usage{
#
plotBoot(object,cost=costBIN,R=200,nclass=13,
sub=paste("Bootstrap (R=",R,")",sep=""),plot=TRUE,...)
#
plotCovPat(object,sub="DX2 and Dbeta",plot=TRUE,...)
#
plotEtaResfunction(object,type="pearson",sub="Residual structure")
#
plotHalfnorm(object,sub="Half-normal plot",type="deviance",env=TRUE,...)
#
plotLeverage(object,type="pearson",sub="Leverage",cex=1.5,pch=20,...)
#
lotObsExp(object,sub="Expected vs observed values",...)
plotObsExpCat(object,sub="Expected vs observed values",horizontal=TRUE,...)
#
plotParRes(object,mgraph=NULL,...)
#
plotQQres(object,type="pearson",sub="QQ-norm for residuals")
#
plotResDens(object,type="pearson",sub="Residuals histogram",nclass=13,...)
}
\arguments{
  \item{object}{an object of class inheriting from 'lm' or glm}
  \item{cost}{}
  \item{R}{}
  \item{nclass}{}
  \item{plot}{}
  \item{sub}{}
  \item{env}{}
  \item{type}{}
  \item{cex}{}
  \item{pch}{}
  \item{horizontal}{}
  \item{mgraph}{}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
The selected plots are drawn on a graphics device.\cr
plotBoot\cr
plotCovPat\cr
lotEtaRes\cr
plotHalfnorm\cr
plotLeverage\cr
plotObsExp: Observed vs Expected values\cr
plotObsExpCat: Observed vs Expected values\cr
plotParRes\cr
plotQQres: Normal QQ-plot of residuals\cr
plotResDens\cr

}
\value{
  x is invisibly returned.
}
\references{
ted\cr
}
\seealso{\code{\link[stats:qqnorm]{qqnorm}},
\code{\link[stats:plot.glm]{plot.glm}}, \code{\link[stats:plot.lm]{plot.lm}}}
\examples{
### plot
}