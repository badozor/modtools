`costRMSE` <-
function (y, yhat = 0) 
sqrt(mean((y - yhat)^2))
