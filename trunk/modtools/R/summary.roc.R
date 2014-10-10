summary.roc <-
function (object, rnd = 3, type="curve", display = TRUE, ...)
{
    if (!inherits(object, "roc"))
        stop("object 'roc' expected !")
    tab <- attr(object, "table")
    lim <- attr(object, "lim")
    mat1 <- as.matrix(tab)
    if (display) {
        cat("------------------------\n")
        cat("Performance Analysis\n")
        cat("------------------------\n")
        cat("cut-off=", round(lim$cut, rnd), "\n")
        cat("sensitivity=", round(lim$sens, rnd), "\n")
        cat("specificity=", round(lim$spec, rnd), "\n")
        cat("positive predictive values=", round(lim$pvp, rnd),
            "\n")
        cat("negative predictive values=", round(lim$pvn, rnd),
            "\n")
        cat("prevalence=", round(lim$prev, rnd),
            "\n")
        w <- ROCcoordinate(object$sens,object$spec,type=type)
        cat("AUC=", round(auc(w$xx, w$yy),
            rnd), "\n")
        cat("Confusion matrix:\n")
        print(tab)
        cat("Kappa=", round(ckappa(mat1), rnd), "\n")
        cat("Good classification=", round(goodclassif(mat1), rnd),
            "\n")
    }
    else return(c(lim, kappa = ckappa(mat1), goodclass = goodclassif(mat1)))
}
