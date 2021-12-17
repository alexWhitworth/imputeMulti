## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @import data.table
## usethis namespace: end
NULL

#' @useDynLib imputeMulti

.onUnload <- function (libpath) {
  library.dynam.unload('imputeMulti', libpath)
}