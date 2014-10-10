\name{SRM}
\alias{SRM}
\title{Structural Risk Minimization}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SRM(y, yhat, h, cost = costRMSE, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{yhat}{ ~~Describe \code{yhat} here~~ }
  \item{h}{ ~~Describe \code{h} here~~ }
  \item{cost}{ ~~Describe \code{cost} here~~ }
  \item{alpha}{ ~~Describe \code{alpha} here~~ }
}
\details{


  + fonction de cout
  
  
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{
Freeman and Hastie\cr
Vapnik\cr
Saporta, G. (2006) Probabilités, analyses des données et statistiques, Second édition, Editions Technip, 622.\cr
}
\note{ ~~further notes~~
+ formule
Remp <- cost(y, yhat)
n <- length(y)
Comp <- sqrt((h * (log(2 * n/h) + 1) - log(alpha/4))/n)
Remp + Comp

\deqn{SRM = R_{emp} + \sqrt{\frac{(h * (log(2 * n/h) + 1)- log(alpha/4))}{n}}}
where n = number of elements, h = complexity measure, alpha = .
\eqn{R_{emp}} is given by the cost function.

}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y, yhat, h, cost = costRMSE, alpha = 0.05) 
{
    Remp <- cost(y, yhat)
    n <- length(y)
    Comp <- sqrt((h * (log(2 * n/h) + 1) - log(alpha/4))/n)
    return(Remp + Comp)
  }
}

