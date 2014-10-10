`costMAE` <-
function (y, yhat = 0) 
mean(abs(y - yhat))
