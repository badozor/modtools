\name{training.dataset}
\Rdversion{1.1}
\alias{training.dataset}
\title{building training and test dataset}
\description{
building training and test dataset
}
\usage{
training.dataset(x, cluster = rep(1, length(x)), ratio = 1/4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{cluster}{
%%     ~~Describe \code{cluster} here~~
}
  \item{ratio}{
%%     ~~Describe \code{ratio} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, cluster = rep(1, length(x)), ratio = 1/4) 
{
    test <- unlist(tapply(x, cluster, function(j) sample(j, round(length(j) * 
        ratio))))
    res <- data.frame(x = x, test = x \%in\% test, training = !(x \%in\% 
        test), cluster = cluster)
    attr(res, "ratio") <- ratio
    attr(res, "N") <- length(x)
    attr(res, "Ntest") <- sum(res$test)
    attr(res, "Ntraining") <- sum(res$training)
    return(res)
  }
}

