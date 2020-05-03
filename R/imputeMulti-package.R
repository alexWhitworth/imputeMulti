## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

#' @useDynLib imputeMulti

.onUnload <- function (libpath) {
  library.dynam.unload('imputeMulti', libpath)
}