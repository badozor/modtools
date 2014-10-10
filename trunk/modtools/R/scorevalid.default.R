# from the course "Analyse discriminante linéaire (au sensde Fisher ou LDA)" proposed by Pierre-André Cornillon
# see http://www.sites.univ-rennes2.fr/laboratoire-statistique/PAC/doc/score.pdf
# and http://www.sites.univ-rennes2.fr/laboratoire-statistique/PAC/index.html
`scorevalid.default` <-
function (y, score, recal = FALSE,qth=seq(0, 1, length = 10),tol=1e-04,...)
{
    if (!all(y %in% c(0, 1))) 
        stop("only binary values!")
    if (length(y) != length(score)) 
        stop("non convenient dimension !")
    if (recal) 
        score <- (score - min(score)) * 100/(max(score) - min(score))
    ordre <- order(score)
    score.ordonne <- score[ordre]
    y.ordonne <- y[ordre]
    decoupage <- quantile(score.ordonne,qth)
    decoupage[1] <- decoupage[1] - tol
    score.decoupage <- cut(score.ordonne, breaks = decoupage)
    table.score <- table(score.decoupage)
    table.y <- table(y.ordonne, score.decoupage)
    px <- cumsum(table.score)/sum(table.score)
    px2 <- rev(cumsum(rev(table.score))/sum(table.score))
    py <- cumsum(table.y[2, ])/cumsum(apply(table.y, 2, sum))
    py2 <- rev(cumsum(rev(table.y[1, ]))/cumsum(rev(apply(table.y,2, sum))))
    p0 <- table(y.ordonne)[1]/sum(table(y.ordonne))
    p1 <- table(y.ordonne)[2]/sum(table(y.ordonne))
    res <- list()
    res$ordre <- ordre
    res$decoupage <- decoupage
    res$y.ordonne <- y.ordonne
    res$table.score <- table.score
    res$table.y <- table.y
    res$px <- px
    res$px2 <- px2
    res$py <- py
    res$py2 <- py2
    res$p0 <- p0
    res$p1 <- p1
    class(res) <- c("scorevalid")
    return(res)
}
