\name{sefit}
\alias{sefit}
\alias{sefit.glm}
\title{Standard error of prediction (or fitted values) from glm}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sefit(object, ...)
\method{sefit}{glm}(object, newdata, interval = "confidence", dispersion = TRUE,m=1,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Object of class inheriting from '"glm"'}
  \item{newdata}{An optional data frame in which to look for variables with which to predict.  If omitted, the data values are used.}
  \item{interval}{a logical value ...}
  \item{dispersion}{Type of interval calculation.}
  \item{m}{a numerical value corresponding to observation number}
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
\author{ ~~who you are~~ }
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    UseMethod("sefit")
  }
}

