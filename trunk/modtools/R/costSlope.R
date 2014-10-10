costSlope <-
function(y, yhat)
coef(lm(y~yhat))[2]
