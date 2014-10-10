\name{scorevalid}
\alias{scorevalid}
\alias{scorevalid.default}
\alias{print.scorevalid}
\alias{plot.scorevalid}
\title{Performance curves}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scorevalid(y, score, ...)
\method{scorevalid}{default}(y, score, recal = FALSE,
  qth=seq(0, 1, length = 10),tol=1e-04,...)
\method{print}{scorevalid}(x,digits = getOption("digits"),...)
\method{plot}{scorevalid}(x,mgraph = NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{score}{ ~~Describe \code{score} here~~ }
  \item{recal}{ ~~Describe \code{recal} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{tol}{1e-04}
  \item{qth}{seq(0, 1, length = 10)}
  \item{mgraph}{graphic organisation}
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
\references{
bardos (2001)\cr
Cornillon P-A (200x) Discrimination et Scores, MASS course - Rennes 2:\cr
http://www.uhb.fr/sc_sociales/labstats/PAC/doc/score.pdf\cr
}
\note{
The functions is based on R code of the excellent document
proposed by Pierre-André Cornillon (see the section 'references').
for more details, you can consult the following links:\cr
http://www.uhb.fr/sc_sociales/labstats/PAC/doc/score.pdf\cr
http://www.uhb.fr/sc_sociales/labstats/PAC/\cr
}
\seealso{\code{\link{roc}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y, score, ...) 
{
    UseMethod("scorevalid")
  }
}


