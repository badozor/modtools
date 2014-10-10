'auc' <-
function(x,y) {
  if(length(x)!=length(y))
    stop("non convient dimensions !")
  res <- 0
  res <- .C("AUC",as.integer(length(x)),as.double(x),as.double(y),as.double(res),PACKAGE="modtools")[[4]]
  return(res)
}

