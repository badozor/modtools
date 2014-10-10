\name{histsim}
\alias{histsim}
\title{Graphical represenation of simulation results}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
histsim(sim, obs, nclass = 10, coeff = 1, ...)
}
\arguments{
  \item{sim}{ ~~Describe \code{sim} here~~ }
  \item{obs}{ ~~Describe \code{obs} here~~ }
  \item{nclass}{ ~~Describe \code{nclass} here~~ }
  \item{coeff}{ ~~Describe \code{coeff} here~~ }
  \item{\dots}{further graphical parameters passed to 'plot.histogram'}
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
function (sim, obs, nclass = 10, coeff = 1, ...) 
{
    r0 <- c(sim, obs)
    h0 <- hist(sim, plot = FALSE, nclass = nclass)
    y0 <- max(h0$counts)
    l0 <- max(sim) - min(sim)
    w0 <- l0/(log(length(sim), base = 2) + 1)
    w0 <- w0 * coeff
    xlim0 <- range(r0) + c(-w0, w0)
    hist(sim, plot = TRUE, nclass = nclass, xlim = xlim0, col = grey(0.8), 
        ...)
    lines(c(obs, obs), c(y0/2, 0))
    points(obs, y0/2, pch = 18, cex = 2)
    invisible()
  }
}


