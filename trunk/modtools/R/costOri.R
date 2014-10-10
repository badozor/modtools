costOri <-
function(y, yhat)
coef(lm(y~yhat))[1]
