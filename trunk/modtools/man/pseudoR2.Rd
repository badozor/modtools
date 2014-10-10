\name{pseudoR2}
\alias{pseudoR2}
\alias{pseudoR2.glm}
\title{Pseudo-R2 for object 'glm'}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pseudoR2(mod0, mod, ...)
\method{pseudoR2}{glm}(mod0, mod, option = "phi", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod0}{ ~~Describe \code{mod0} here~~ }
  \item{mod}{ ~~Describe \code{mod} here~~ }
  \item{option}{ ~~Describe \code{option} here~~ }
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
\references{Mcfadden 1973, estrella 1998}
\note{ ~~further notes~~

}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mod0, mod, ...) 
{
    UseMethod("pseudoR2")
  }
}


