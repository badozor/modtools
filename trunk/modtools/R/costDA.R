costDA <-
function(y,yhat,cutoff=0.5){
  w <- as.matrix(table(y,yhat > cutoff))
  goodclassif(w)
}
