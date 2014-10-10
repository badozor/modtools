\name{cost}
\alias{cost}
\alias{costAVER}
\alias{costBIN}
\alias{costMAE}
\alias{costMSE}
\alias{costRMSE}
\alias{costKappa}
\alias{costGoodCl}
\alias{costSlope}
\alias{costOri}
\alias{costR2}
\title{Cost functions}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
costAVER(y, yhat = 0)
costMAE(y, yhat = 0)
costMSE(y, yhat = 0)
costRMSE(y, yhat = 0)
costSlope(y, yhat)
costOri(y, yhat)
costR2(y, yhat)
costBIN(y, mu = 0, cutoff = 0.5)
costKappa(y, yhat, cutoff = 0.5)
costGoodCl(y,yhat, cutoff = 0.5)
}
\arguments{
  \item{y}{a numerical vector corresponding to the obsevred values.}
  \item{yhat}{a numerical vector corresponding to the expected values.}
  \item{mu}{a numerical vector corresponding to the expected values.}
  \item{cutoff}{a numerical value in the range [0,1] corresponding to the
  threshold to transform the values 'mu' into binary vector.}
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
\references{Davidson and Hinkley, Saporta 2006}
\note{ ~~further notes~~

add descriptions and formula \cr
}
\seealso{\code{\link{SRM}},\code{\link{bootvalid}},\code{\link[boot:cv.glm]{cv.glm}}}
\examples{
x <- rnorm(20,2,5)
y <- -6+x*3+rnorm(20)
lm1 <- lm(y~x)
costRMSE(y,lm1$fitted)
}

