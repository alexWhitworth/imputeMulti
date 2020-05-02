## usethis namespace: start
#' @importFrom Rcpp sourceCpp, evalCpp
## usethis namespace: end
NULL

#' @useDynLib imputeMulti

.onUnload <- function (libpath) {
  library.dynam.unload('imputeMulti', libpath)
}