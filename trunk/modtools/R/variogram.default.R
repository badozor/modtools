variogram.default <- function (x, dis, type = "deviance", add.lowess = TRUE, exp.adj = TRUE, 
		sub = "Variogram", breaks = c(-0.01, seq(min(dis), max(dis), 
						by = 5)), xlim = c(min(dis), max(dis)), verbose = TRUE, 
		gammatol = 0.9, add.sd = TRUE, level = 0.9, start = list(c0 = 0.01, 
				c1 = 0.7, scal = 15), subset = 2:c(xlim[2] - 1), ...) 
{
	varexp <- function(h, c0, c1, scal) {
		f <- c0 + c1 * (1 - exp(-h/scal))
		f[h == 0] <- 0
		return(f)
	}
	try.test <- function(x) !inherits(x, "try-error")
	z2 <- (dist(x)^2)/2
	distclas <- cut(dis, breaks = breaks)
	distvario1 <- tapply(dis, distclas, mean)
	vario1 <- tapply(z2, distclas, mean, na.rm = TRUE)
	nbcouples1 <- tapply(z2, distclas, length)
	vario1d <- cbind(vario1, distvario1, nbcouples1)
	vario1d <- na.omit(vario1d)
	vario1dat <- as.data.frame(vario1d)
	varioajust1 <- try(nls(vario1 ~ varexp(distvario1, c0, c1, 
							scal), data = vario1dat, start = start, subset = subset))
	if (try.test(varioajust1) & exp.adj) {
		c0a <- coef(varioajust1)[1]
		c1a <- coef(varioajust1)[2]
		scala <- coef(varioajust1)[3]
		xx <- seq(0.01, xlim[2], 1)
		vv <- varexp(seq(0.01, xlim[2], 1), c0a, c1a, scala)
		g1a <- c1a + c0a
		seuil1 <- gammatol * g1a
		lag1 <- sort(xx[vv >= seuil1])[1]
	}
	plot(vario1d[1:nrow(vario1d), 2:1], type = "n", pch = 20, 
			cex = 2, panel.first = c(grid()), xlim = xlim, xlab = "Distance", 
			ylab = "Semivariance", ...)
	if (add.sd) {
		sdvario1 <- tapply(z2, distclas, sd, na.rm = TRUE)
		lwr <- vario1 - (sdvario1/sqrt(nbcouples1)) * qt(1 - 
						(1 - level)/2, nbcouples1 - 1)
		upr <- vario1 + (sdvario1/sqrt(nbcouples1)) * qt(1 - 
						(1 - level)/2, nbcouples1 - 1)
		polygon(cbind(c(distvario1, rev(distvario1)), c(lwr, 
								rev(upr))), border = FALSE, col = grey(0.8))
		lines(distvario1, lwr, lty = 2, col = grey(0.4))
		lines(distvario1, upr, lty = 2, col = grey(0.4))
	}
	lines(vario1d[1:nrow(vario1d), 2:1], type = "b", pch = 20, 
			cex = 2)
	if (add.lowess) {
		w <- na.omit(data.frame(y = vario1d[1:nrow(vario1d), 
								1], x = vario1d[1:nrow(vario1d), 2]))
		lines(lowess(w$y ~ w$x), col = "red", lwd = 2)
	}
	if (verbose & try.test(varioajust1) & exp.adj) {
		print(summary(varioajust1))
		cat("\nEstimated Lag distance: ", lag1, "\n", sep = "")
		cat("Estimated Threshold: ", seuil1, "\n", sep = "")
		cat("Theoretical gamma: ", g1a, "\n\n", sep = "")
	}
	if (try.test(varioajust1) & exp.adj) {
		lines(xx, vv, col = "dark green", lwd = 2)
		abline(h = c0a + c1a, lty = 3, col = "blue")
		abline(v = lag1, lty = 3, col = "blue")
	}
	title(sub)
}

