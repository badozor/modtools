\name{roc}
\alias{roc}
\alias{prep.roc}
\alias{plot.roc}
\alias{print.roc}
\alias{summary.roc}
\alias{ROCcoordinate}
\alias{optimCut}
\alias{fgoodclassif}
\alias{fkappa}
\alias{fSpecSens}
\alias{fSpecSens2}
\title{ROC functions}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prep.roc(obs,pred,nbval=20,method="max",subset,...)
\method{plot}{roc}(x, type = "curve", sub,posi = c(0.8, 0.2),...)
\method{print}{roc}(x, ...)
\method{summary}{roc}(object, rnd = 3, type = "curve", display = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{ ~~Describe \code{obs} here~~ }
  \item{pred}{ ~~Describe \code{pred} here~~ }
  \item{nbval}{ ~~Describe \code{nbval} here~~ }
  \item{subset}{ ~~Describe \code{subset} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{sub}{ ~~Describe \code{sub} here~~ }
  \item{object}{ ~~Describe \code{object} here~~ }
  \item{type}{ ~~Describe \code{method} here~~ }
  \item{posi}{information position}
  \item{rnd}{ ~~Describe \code{rnd} here~~ }
  \item{display}{ ~~Describe \code{display} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\note{
With the option 'estim', the function prep.roc used the function OptimCut to define the optimal cut-off.
this one can be based on several criteria: \cr
fgoodclassif:\cr
fkappa\cr
fSpecSens\cr
fSpecSens2\cr
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
x <- rnorm( 100 )
z <- rnorm( 100 )
w <- rnorm( 100 )
tigol <- function( x ) 1 - ( 1 + exp( x ) )^(-1)
y <- rbinom( 100, 1, tigol( 0.3 + 3*x + 5*z + 7*w ) )
ROC( form = y ~ x + z, plot="ROC" )
glm1 <- glm(y ~ x + z,family=binomial)
roc1 <- prep.roc(glm1$y,glm1$fitted)
plot(roc1)
}

