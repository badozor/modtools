plot.roc <- function (x, type = "curve", sub, posi = c(0.8, 0.2),lty=1,lwd = 2, pch = 19, pt.cex = 1,lgd.cex=0.9,...) 
{
	if (!inherits(x, "roc")) 
		stop("object 'roc' expected !")
	if (missing(sub)) 
		sub <- "ROC curve"
	w <- ROCcoordinate(x$sens, x$spec, type = type)
	xx <- w$xx
	yy <- w$yy
	plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "1-specificity", 
			ylab = "sensitivity", type = "n", panel.first = c(grid(), 
					abline(0, 1, lwd = 1, lty = 2, col = "gray")), ...)
	lines(xx, yy,lty=lty,lwd=lwd)
	cut1 <- attr(x, "lim")
	points(1 - cut1$spec, cut1$sens, pch = pch, cex = pt.cex, col = "red")
	label1 <- paste("cutoff: ", round(cut1$cut, 2), "\nsens: ", 
			round(cut1$sens, 2), "\nspec: ", round(cut1$spec, 2), 
			"\npv+: ", round(cut1$pvp, 2), "\npv-: ", round(cut1$pvn, 
					2), "\nauc: ", round(auc(xx, yy), 2), sep = "")
	text(posi[1], posi[2], label = label1, cex = lgd.cex, col = "red")
	title(sub)
}