rmse <- function(pred, obs) {
  sqrt(mean((pred
             - obs) ^ 2))
}

get_correct_preds <- function(a) {
  ir <- match(rownames(a), colnames(a))
  ic <- match(colnames(a), rownames(a))
  res <- NULL
  if(length(ir) <= length(ic)){
    for(idx in 1:length(ir)){
      res <- c(res, a[ic[ir[idx]],ir[idx]])
    }
  } else {
    for(idx in 1:length(ic)){
      res <- c(res, a[ic[idx], ir[ic[idx]]])
    }
  }
  return(res)
}