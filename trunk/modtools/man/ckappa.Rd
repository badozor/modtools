\name{ckappa}
\alias{ckappa}
\title{Kappa's index}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ckappa(x)
}
\arguments{
  \item{x}{a matrix }
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


}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    if (ncol(x) != nrow(x)) 
        stop("non covenient dimension !")
    N <- sum(x)
    sxii <- sum(diag(x))
    sxip <- apply(x, 2, sum)
    sxpi <- apply(x, 1, sum)
    k <- (N * sxii - sum(sxip * sxpi))/(N * N - sum(sxip * sxpi))
    return(k)
  }
}

