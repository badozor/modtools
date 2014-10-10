\name{intervals}
\alias{intervals}
\alias{intervals.glm}
\alias{intervals.robclust}
\title{confidence and prediction/tolerance intervals for glm}
\description{
This method gives confidence and prediction/tolerance intervals for the expected values from a generalized linear model (object of class 'glm').
}
\usage{
intervals(object, ...)
\method{intervals}{glm}(object, newdata, type = "response", interval = "confidence",
method = 1, level = 0.05, ...)
}
\arguments{
  \item{object}{Object of class inheriting from '"glm"'}
  \item{newdata}{An optional data frame in which to look for variables with which to predict.  If omitted, the data values are used.}
  \item{type}{the type of prediction required (by defaut type="response")}
  \item{interval}{Type of interval calculation.}
  \item{method}{a numerical values (by default method=1). The option 'method = 1' gives intervals
  based on the carry-over of the extreme values. The option 'method = 2' provides "direct" interval (see the section note for more details.}
  \item{level}{Tolerance/confidence level}
  \item{\dots}{further arguments passed to or from other methods}
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
\note{ ~~further notes~~
Several procedure can provide confidence (or prediction/tolerance) intervals. In the function intervals.glm,
we propose the two following procedures:
method 1: confidence and prediction intervals based on the carry-over of the extreme values.
This method is an extrapolation of the results obtained in the linear model.
for confidence intervals
\deqn{sigma = \sqrt{x^tVCOVx}}
where
for prediction intervals
\deqn{sigma = \sqrt{1+x^tVCOVx}}
where
method 2: "direct" confidence intervals
\deqn{\hat{y} +- epsilon_{alpha}var(\hat{y})\sqrt{x^tVCOVx}}
where \eqn{var(\hat{y})=psi var()}
}
\seealso{\code{\link[stats:glm]{glm}},\code{\link[stats:predict.glm]{predict.glm}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    UseMethod("intervals")
  }
}


