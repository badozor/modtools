pkgname <- "modtools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('modtools')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("SRM")
### * SRM

flush(stderr()); flush(stdout())

### Name: SRM
### Title: Structural Risk Minimization
### Aliases: SRM

### ** Examples

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



cleanEx()
nameEx("anscresid")
### * anscresid

flush(stderr()); flush(stdout())

### Name: anscresid
### Title: Anscombe's Residuals
### Aliases: anscresid

### ** Examples

## binomial

## poisson

## Gamma




cleanEx()
nameEx("auc")
### * auc

flush(stderr()); flush(stdout())

### Name: auc
### Title: Area Under Curves
### Aliases: auc

### ** Examples

# y <- rpois(1:10)
# d1 <- density(y)
# auc(d1$x,d1$y)



cleanEx()
nameEx("bootcoef")
### * bootcoef

flush(stderr()); flush(stdout())

### Name: bootcoef
### Title: ~~function to do ... ~~
### Aliases: bootcoef

### ** Examples


# add an example




cleanEx()
nameEx("bootcoef.ci")
### * bootcoef.ci

flush(stderr()); flush(stdout())

### Name: bootcoef.ci
### Title: ~~function to do ... ~~
### Aliases: bootcoef.ci

### ** Examples


# add an example




cleanEx()
nameEx("bootvalid")
### * bootvalid

flush(stderr()); flush(stdout())

### Name: bootvalid
### Title: Validation procedure based on bootstrap for object from 'glm' or
###   'lm'.
### Aliases: bootvalid bootvalid.default summary.bootcorrected

### ** Examples

## glm1

## bootvalid

## histogram of results




cleanEx()
nameEx("ckappa")
### * ckappa

flush(stderr()); flush(stdout())

### Name: ckappa
### Title: Kappa's index
### Aliases: ckappa

### ** Examples

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



cleanEx()
nameEx("cost")
### * cost

flush(stderr()); flush(stdout())

### Name: cost
### Title: Cost functions
### Aliases: cost costAVER costBIN costMAE costMSE costRMSE costKappa
###   costGoodCl costSlope costOri costR2

### ** Examples

x <- rnorm(20,2,5)
y <- -6+x*3+rnorm(20)
lm1 <- lm(y~x)
costRMSE(y,lm1$fitted)



cleanEx()
nameEx("goodclassif")
### * goodclassif

flush(stderr()); flush(stdout())

### Name: goodclassif
### Title: Good classification
### Aliases: goodclassif

### ** Examples

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



cleanEx()
nameEx("hist.boot")
### * hist.boot

flush(stderr()); flush(stdout())

### Name: hist.boot
### Title: Graphical representation of object 'boot'
### Aliases: hist.boot

### ** Examples

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



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("histsim")
### * histsim

flush(stderr()); flush(stdout())

### Name: histsim
### Title: Graphical represenation of simulation results
### Aliases: histsim

### ** Examples

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



cleanEx()
nameEx("intervals")
### * intervals

flush(stderr()); flush(stdout())

### Name: intervals
### Title: confidence and prediction/tolerance intervals for glm
### Aliases: intervals intervals.glm intervals.robclust

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    UseMethod("intervals")
  }



cleanEx()
nameEx("modperf")
### * modperf

flush(stderr()); flush(stdout())

### Name: modperf
### Title: Model performance
### Aliases: modperf modperf.cv modperf.boot modperf.lm modperf.glm
###   modperf.binary modperf.default perfbinary

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    UseMethod("modperf")
  }



cleanEx()
nameEx("modplot")
### * modplot

flush(stderr()); flush(stdout())

### Name: modplot
### Title: Plot Diagnostics for glm and lm Objects
### Aliases: plotBoot plotCovPat plotEtaRes plotHalfnorm plotLeverage
###   plotObsExp plotObsExpCat plotParRes plotQQres plotResDens
###   envelope.bin

### ** Examples

### plot



cleanEx()
nameEx("modtools-package")
### * modtools-package

flush(stderr()); flush(stdout())

### Name: modtools-package
### Title: Additional tools for model diagnostic and selection
### Aliases: modtools-package modtools

### ** Examples

# nothing for the moment



cleanEx()
nameEx("pseudoR2")
### * pseudoR2

flush(stderr()); flush(stdout())

### Name: pseudoR2
### Title: Pseudo-R2 for object 'glm'
### Aliases: pseudoR2 pseudoR2.glm

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mod0, mod, ...) 
{
    UseMethod("pseudoR2")
  }



cleanEx()
nameEx("roc")
### * roc

flush(stderr()); flush(stdout())

### Name: roc
### Title: ROC functions
### Aliases: roc prep.roc plot.roc print.roc summary.roc ROCcoordinate
###   optimCut fgoodclassif fkappa fSpecSens fSpecSens2

### ** Examples

x <- rnorm( 100 )
z <- rnorm( 100 )
w <- rnorm( 100 )
tigol <- function( x ) 1 - ( 1 + exp( x ) )^(-1)
y <- rbinom( 100, 1, tigol( 0.3 + 3*x + 5*z + 7*w ) )
# need update
# ROC( form = y ~ x + z, plot="ROC" )
glm1 <- glm(y ~ x + z,family=binomial)
roc1 <- prep.roc(glm1$y,glm1$fitted)
plot(roc1)



cleanEx()
nameEx("scorevalid")
### * scorevalid

flush(stderr()); flush(stdout())

### Name: scorevalid
### Title: Performance curves
### Aliases: scorevalid scorevalid.default print.scorevalid plot.scorevalid

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y, score, ...) 
{
    UseMethod("scorevalid")
  }



cleanEx()
nameEx("sefit")
### * sefit

flush(stderr()); flush(stdout())

### Name: sefit
### Title: Standard error of prediction (or fitted values) from glm
### Aliases: sefit sefit.glm

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    UseMethod("sefit")
  }



cleanEx()
nameEx("training.dataset")
### * training.dataset

flush(stderr()); flush(stdout())

### Name: training.dataset
### Title: building training and test dataset
### Aliases: training.dataset

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, cluster = rep(1, length(x)), ratio = 1/4) 
{
    test <- unlist(tapply(x, cluster, function(j) sample(j, round(length(j) * 
        ratio))))
    res <- data.frame(x = x, test = x %in% test, training = !(x %in% 
        test), cluster = cluster)
    attr(res, "ratio") <- ratio
    attr(res, "N") <- length(x)
    attr(res, "Ntest") <- sum(res$test)
    attr(res, "Ntraining") <- sum(res$training)
    return(res)
  }



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
