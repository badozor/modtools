\name{anscresid}
\alias{anscresid}
\title{Anscombe's Residuals}
\description{
The fonction provides Anscombe's residuals associated with an object 'glm'.
}
\usage{
anscresid(object, ...)
}
\arguments{
  \item{object}{Object of class inheriting from '"glm"'}
  \item{\dots}{further arguments passed to or from other methods}
}
\details{
The formulas to compute the Anscombe'sresiduals are defined as follow:is defined as follow:
for gaussian family:\cr
\deqn{r_{ans} = y - \mu}
for inverse.gaussian family:\cr
\deqn{r_{ans} = (log(y) - log(\mu))/(\mu^{0.5})}
for binomial family:\cr
\deqn{r_{ans} = \sqrt(m) * (b(y) - b(\mu)) * (\mu * (1-\mu))^{-1/6}}
\deqn{ b(x) = \int_0^x{(x^{-1/3} * (1 - x)^{-1/3}}}
for poisson family:\cr
\deqn{r_{ans} = (3/2) * ((y^{2/3}) * \mu^{-1/6} - \mu^{0.5})}
for Gamma family:\cr
\deqn{r_{ans} = 3 * ((y/\mu)^{1/3} - 1)}
}
\value{
The function returns a numerical vector which contains the values of anscombe's residuals for each observation.

}
\references{
McCullagh P. and Nelder, J. A. (1989) Generalized Linear Models. London: Chapman and Hall.\cr
Pierce, D. A. and Schafer, D. W. (1986) Residuals in Generalized Linear Models, Journal of the American Statistical Association, \bold{81},396,977-986.\cr
Abscombe 1953\cr
}
\seealso{\code{\link[stats:glm]{glm}},\code{\link[stats:residuals.glm]{residuals.glm}}}
\examples{
## binomial

## poisson

## Gamma

}
