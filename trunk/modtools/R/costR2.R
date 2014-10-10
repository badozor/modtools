`costR2` <-
function(y, yhat)
summary(lm(y~yhat))$r.squared
