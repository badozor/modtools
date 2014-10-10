`costBIN` <-
function (y, mu = 0, cutoff = 0.5) 
mean(abs(y - mu) > cutoff)
