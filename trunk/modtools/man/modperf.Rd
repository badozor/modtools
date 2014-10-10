\name{modperf}
\Rdversion{1.1}
\alias{modperf}
\alias{modperf.cv}
\alias{modperf.boot}
\alias{modperf.lm}
\alias{modperf.glm}
\alias{modperf.binary}
\alias{modperf.default}
\alias{perfbinary}
\title{Model performance}
\description{Model performance
}
\usage{
modperf(x, ...)
modperf.boot(x, data, cost = costRMSE, R = 99, ...)
modperf.cv(x, data, cost = costRMSE, K = 10, ...)
modperf.binary(x,...)
\method{modperf}{default}(x, data, cost = costRMSE, R = 99, ...)
\method{modperf}{lm}(x, ...)
\method{modperf}{glm}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{cost}{
%%     ~~Describe \code{cost} here~~
}
  \item{R}{
%%     ~~Describe \code{R} here~~
}
  \item{K}{
%%     ~~Describe \code{K} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
The function 'modperf.binary' is based on the function 'perf.binary'.
this function returns the following values: sensitivity,specificty,
positive predictive value, negative predictive value and prevalence.\cr
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    UseMethod("modperf")
  }
}
