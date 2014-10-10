\name{hist.boot}
\alias{hist.boot}
\title{Graphical representation of object 'boot'}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{hist}{boot}(x, nclass = 10, coeff = 1, mfrow = NULL, which.par = 1:length(x$t0),
sub = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class '"boot"' containing the output of a bootstrap calculation.}
  \item{nclass}{numeric (integer).'nclass'is equivalent to 'breaks' for a scalar or character argument.}
  \item{coeff}{ ~~Describe \code{coeff} here~~ }
  \item{mfrow}{ ~~Describe \code{mfrow} here~~ }
  \item{which.par}{ ~~Describe \code{which.par} here~~ }
  \item{sub}{ ~~Describe \code{sub} here~~ }
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
function (x, nclass = 10, coeff = 1, mfrow = NULL, which.par = 1:length(x$t0), 
    sub = NULL, ...) 
{
    if (!inherits(x, "boot")) 
        stop("non convenient argument")
    opar <- par(ask = par("ask"), mfrow = par("mfrow"))
    on.exit(par(opar))
    if (is.null(mfrow)) 
        mfrow <- n2mfrow(length(which.par))
    par(mfrow = mfrow)
    if (length(which.par) > prod(mfrow)) 
        par(ask = TRUE)
    for (i in which.par) {
        if (is.null(sub)) 
            sub <- paste("t", i, "*", sep = "")
        obs <- x$t0[i]
        sim <- x$t[, i]
        r0 <- c(sim, obs)
        h0 <- hist(sim, plot = FALSE, nclass = nclass)
        y0 <- max(h0$counts)
        l0 <- max(sim) - min(sim)
        w0 <- l0/(log(length(sim), base = 2) + 1)
        w0 <- w0 * coeff
        xlim0 <- range(r0) + c(-w0, w0)
        hist(sim, plot = TRUE, nclass = nclass, xlim = xlim0, 
            col = grey(0.8), main = sub, ...)
        lines(c(obs, obs), c(y0/2, 0))
        points(obs, y0/2, pch = 18, cex = 2)
    }
    invisible()
  }
}

