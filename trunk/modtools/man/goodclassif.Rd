\name{goodclassif}
\alias{goodclassif}
\title{Good classification}
\description{
This function gives the percentage of good classification in confusion matrix.
}
\usage{
goodclassif(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a matrix or data.frame}
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
+ description des matrices de confusion

sum de la diagonale

}
\seealso{\code{\link{kappa}},\code{\link{roc}}}
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
    return(sxii/N)
  }
}

