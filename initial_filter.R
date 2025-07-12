pval_cv_thres <- function(data, cvthres, pthres) {
  pcols <- grep("Adj\\. P-Value", colnames(data))
  cvcols <- grep("CV", colnames(data))
  pfilt <- data[apply(data[, pcols], 1, function(x) any(x < pthres, na.rm = T)),]
  cvfilt <- pfilt[apply(pfilt[, cvcols], 1, function(x) any(x < cvthres, na.rm = T)),]
  return(cvfilt)
}
