\name{auc}
\alias{auc}
\title{Area Under Curves}
\description{
This function compute the area under curves.
}
\usage{
auc(x,y)
}
\arguments{
  \item{x}{a numeric vector}
  \item{y}{a numeric vector}
}
\value{
The function returns a numeric value corresponding to the area under the curves.
}
\seealso{}
\examples{
# y <- rpois(1:10)
# d1 <- density(y)
# auc(d1$x,d1$y)
}

