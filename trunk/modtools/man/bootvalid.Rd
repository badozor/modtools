\name{bootvalid}
\alias{bootvalid}
\alias{bootvalid.default}
\alias{summary.bootcorrected}
\title{Validation procedure based on bootstrap for object from 'glm' or 'lm'.}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bootvalid(object, ...)
\method{bootvalid}{default}(object, data, cost = costMSE,
R = 99,method = "raw",...)
\method{summary}{bootcorrected}(object,display=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Object of class inheriting from '"glm"'}
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{cost}{ ~~Describe \code{cost} here~~ }
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{method}{'raw' or 'corrected'}
  \item{display}{a logical value (by default, display=TRUE)}
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
\references{
Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application.
Cambridge University Press.\cr
Efron et al.\cr
Efron et al.\cr
Harrel 2001\cr
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
## glm1

## bootvalid

## histogram of results

}

